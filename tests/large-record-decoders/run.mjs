// Regression test for the generated flags/port decoders of very large records.
//
// The compiler generates the decoder for `main`'s flags and for incoming
// ports. That decoder must stay parseable by JS engines with small parser
// stacks (iOS Safari gave up at ~334 levels of nested functions), no matter
// how many fields the record has, and it must keep decoding to the same
// values with the same errors.
//
// Usage: node tests/large-record-decoders/run.mjs <path to elm binary> [field count]
//
// Without a field count it runs a set of counts that between them use every
// arity of Json.Decode.map..map8, plus the empty record.

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { pathToFileURL } from "node:url";
import vm from "node:vm";

// A setup problem, not a test failure: exits with 2 so it isn't mistaken for
// the bug.
const setupFailure = (message) => {
  console.log(message);
  process.exit(2);
};

const parseFieldCount = (arg) => {
  if (!/^\d+$/.test(arg)) {
    setupFailure(`field count must be a non-negative integer, got ${JSON.stringify(arg)}`);
  }
  return Number(arg);
};

// `make` links the built compiler as `whelm` in the repo root.
const elm = path.resolve(process.argv[2] ?? "whelm");
const fieldCounts =
  process.argv[3] === undefined
    ? [0, 1, 3, 4, 6, 7, 9, 65, 1000]
    : [parseFieldCount(process.argv[3])];

// Node's default is 984 KB. At 600 KB V8 parses roughly 270 levels of the
// nested `andThen(function (x) { ... })` shape, below the ~334 that iOS
// manages, so passing here leaves headroom on iOS too.
const parseStackKb = 600;

// Field 1 is named `await`, a reserved word in ES modules, so that the .mjs
// output shows whether field names leak into JS identifiers.
const fieldName = (i) => (i === 1 ? "await" : "f" + String(i).padStart(5, "0"));

const fieldType = (i) => ["Int", "String", "List Int"][i % 3];

const fieldValue = (i) => [i, "s" + i, [i, i + 1]][i % 3];

// Fields that the error checks break: the first and the last Int field
// (every third field is an Int, see fieldType). Empty without Int fields.
const brokenFields = (fieldCount) => {
  const ints = Array.from({ length: fieldCount }, (_, i) => i).filter((i) => i % 3 === 0);
  return [...new Set([ints[0], ints[ints.length - 1]])].filter((i) => i !== undefined);
};

// Messages recorded from the compiler before the decoder change. With several
// broken fields the flags error names the one with the last name, because
// fields are decoded last name first.
const expectedFlagsError = (broken) =>
  "Error: Problem with the flags given to your Elm program on initialization.\n\n" +
  `Problem with the value at json.big.${fieldName(broken[broken.length - 1])}:\n\n` +
  '    {\n        "wrong": true\n    }\n\n' +
  "Expecting an INT";

const expectedPortError =
  "Error: Trying to send an unexpected type of value through port `bigIn`:\n[object Object]";

const bigValue = (fieldCount) =>
  Object.fromEntries(
    Array.from({ length: fieldCount }, (_, i) => [fieldName(i), fieldValue(i)]),
  );

const elmSource = (fieldCount) => {
  const fields = Array.from(
    { length: fieldCount },
    (_, i) => `${fieldName(i)} : ${fieldType(i)}`,
  );
  return `port module Main exposing (main)


type alias Big =
    { ${fields.join("\n    , ")}
    }


type alias Flags =
    { label : String
    , big : Big
    }


type Msg
    = EchoFlags
    | GotBig Big


port echoFlags : (() -> msg) -> Sub msg


port bigIn : (Big -> msg) -> Sub msg


port out : { source : String, big : Big } -> Cmd msg


main : Program Flags Flags Msg
main =
    Platform.worker
        { init = \\flags -> ( flags, Cmd.none )
        , update = update
        , subscriptions = \\_ -> Sub.batch [ echoFlags (\\() -> EchoFlags), bigIn GotBig ]
        }


update : Msg -> Flags -> ( Flags, Cmd Msg )
update msg flags =
    case msg of
        EchoFlags ->
            -- A local named like a word reserved in ES modules.
            let
                await =
                    flags.label
            in
            ( flags, out { source = await, big = flags.big } )

        GotBig big ->
            ( flags, out { source = "port", big = big } )
`;
};

const elmJson = {
  type: "application",
  "source-directories": ["src"],
  "elm-version": "0.19.2",
  dependencies: {
    direct: { "elm/core": "1.0.2", "elm/json": "1.1.2" },
    indirect: {},
  },
  "test-dependencies": { direct: {}, indirect: {} },
};

// JSON with object keys sorted, so key order doesn't matter. Also works on
// values from the vm context, where deep-equality checks trip over prototypes.
const canonical = (v) =>
  JSON.stringify(v, (_, x) =>
    x && typeof x === "object" && !Array.isArray(x)
      ? Object.fromEntries(Object.keys(x).sort().map((k) => [k, x[k]]))
      : x,
  );

const assertEqual = (actual, expected, what) => {
  const a = canonical(actual);
  const e = canonical(expected);
  if (a !== e) {
    const clip = (s) => (s.length > 300 ? s.slice(0, 300) + "…" : s);
    throw new Error(`${what}:\n  expected ${clip(e)}\n  actual   ${clip(a)}`);
  }
};

const compile =(dir, extraArgs, output) => {
  const r = spawnSync(elm, ["make", "src/Main.elm", "--output=" + output, ...extraArgs], {
    cwd: dir,
    encoding: "utf8",
  });
  if (r.error) {
    setupFailure(`could not run ${elm}: ${r.error.message}`);
  }
  if (r.status !== 0) {
    setupFailure(
      `elm make ${extraArgs.join(" ")} failed (generated program kept in ${dir}):\n` +
        `${r.stdout}${r.stderr}`,
    );
  }
  return path.join(dir, output);
};

// node --check parses a .mjs file as an ES module and a .js file as a script.
const parsesWithSmallStack = (file) => {
  const r = spawnSync(process.execPath, [`--stack-size=${parseStackKb}`, "--check", file], {
    encoding: "utf8",
  });
  if (r.status !== 0) {
    const firstLine = (r.stderr.match(/^\w*Error.*$/m) ?? [r.stderr.slice(0, 200)])[0];
    throw new Error(`does not parse with --stack-size=${parseStackKb}: ${firstLine}`);
  }
};

// Loads a compiled script in a fresh context. Uses the default stack, which
// the decoders must also fit in at runtime.
const loadScript = async (file) => {
  const context = vm.createContext({ console, setTimeout, clearTimeout });
  new vm.Script(fs.readFileSync(file, "utf8"), { filename: file }).runInContext(context);
  return context.Elm.Main;
};

const loadModule = async (file) => (await import(pathToFileURL(file).href)).Elm.Main;

const startWith = (Main, flags) => {
  const sent = [];
  const app = Main.init({ flags });
  app.ports.out.subscribe((v) => sent.push(v));
  return { app, sent };
};

const flushed = () => new Promise((resolve) => setTimeout(resolve, 0));

const errorOf = (fn) => {
  try {
    fn();
  } catch (e) {
    return e instanceof Error ? e.message : String(e);
  }
  throw new Error("expected an exception, got none");
};

// Returns whether every check passed.
const runFor = async (fieldCount) => {
  const results = [];
  const check = (name, fn) => {
    try {
      fn();
      results.push({ name, ok: true });
    } catch (e) {
      results.push({ name, ok: false, why: e instanceof Error ? e.message : String(e) });
    }
  };

  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "large-record-decoders-"));
  fs.mkdirSync(path.join(dir, "src"));
  fs.writeFileSync(path.join(dir, "elm.json"), JSON.stringify(elmJson, null, 4));
  fs.writeFileSync(path.join(dir, "src", "Main.elm"), elmSource(fieldCount));

  const dev = compile(dir, [], "dev.js");
  const optimized = compile(dir, ["--optimize"], "optimized.js");
  const devModule = compile(dir, [], "dev.mjs");
  const optimizedModule = compile(dir, ["--optimize"], "optimized.mjs");

  check("dev output parses with a small stack", () => parsesWithSmallStack(dev));
  check("--optimize output parses with a small stack", () => parsesWithSmallStack(optimized));
  check("dev ES module parses with a small stack", () => parsesWithSmallStack(devModule));
  check("--optimize ES module parses with a small stack", () =>
    parsesWithSmallStack(optimizedModule),
  );

  const big = bigValue(fieldCount);

  // Returns the running app, or undefined if the output didn't load.
  const checkDecoding = async (mode, load, file) => {
    let Main;
    let loadError;
    try {
      Main = await load(file);
    } catch (e) {
      loadError = e;
    }
    check(`${mode} output loads`, () => {
      if (loadError) throw loadError;
    });
    if (!Main) return undefined;

    const { app, sent } = startWith(Main, { label: "flags", big });
    app.ports.echoFlags.send(null);
    await flushed();
    check(`${mode}: flags decode to the given values`, () =>
      assertEqual(sent, [{ source: "flags", big }], "sent on out"),
    );

    sent.length = 0;
    app.ports.bigIn.send(big);
    await flushed();
    check(`${mode}: port decodes to the given values`, () =>
      assertEqual(sent, [{ source: "port", big }], "sent on out"),
    );
    return { Main, app };
  };

  const devRun = await checkDecoding("dev", loadScript, dev);
  await checkDecoding("--optimize", loadScript, optimized);
  await checkDecoding("dev ES module", loadModule, devModule);
  await checkDecoding("--optimize ES module", loadModule, optimizedModule);

  // Only the dev build's messages say which field failed, and a record
  // without Int fields has nothing to break.
  const broken = brokenFields(fieldCount);
  if (devRun && broken.length > 0) {
    const { Main, app } = devRun;
    const breakFields = (value) => {
      const result = { ...value };
      for (const i of broken) result[fieldName(i)] = { wrong: true };
      return result;
    };

    // The message names the field that failed first, so it also pins the order
    // fields are decoded in.
    check("bad flags give the expected error", () =>
      assertEqual(
        errorOf(() => Main.init({ flags: { label: "flags", big: breakFields(big) } })),
        expectedFlagsError(broken),
        "error",
      ),
    );

    check("bad port value gives the expected error", () =>
      assertEqual(
        errorOf(() => app.ports.bigIn.send(breakFields(big))),
        expectedPortError,
        "error",
      ),
    );
  }

  console.log(`== ${fieldCount} fields`);
  for (const r of results) {
    console.log(
      `${r.ok ? "PASS" : "FAIL"}  ${r.name}${r.ok ? "" : "\n      " + r.why.replace(/\n/g, "\n      ")}`,
    );
  }
  const ok = results.every((r) => r.ok);
  if (ok) {
    fs.rmSync(dir, { recursive: true });
  } else {
    console.log(`(generated program kept in ${dir})`);
  }
  return ok;
};

if (!fs.existsSync(elm)) {
  setupFailure(
    `no elm binary at ${elm}: run \`make\` in the repo root, or pass the binary as the first argument`,
  );
}

let allOk = true;
for (const fieldCount of fieldCounts) {
  allOk = (await runFor(fieldCount)) && allOk;
}
process.exit(allOk ? 0 : 1);
