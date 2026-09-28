import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  blueprintHash,
  blueprintSourceHash,
  buildRecordPath,
  checkBlueprintStamp,
} from "../demo/scripts/lib/blueprint-stamp.mjs";
import { EXIT, main } from "./sync-blueprint-from.mjs";

const pin = "aiken v1.1.23+5adf783";
const digests = {
  "preprod-testing": "a".repeat(64),
  "local-devnet-testing": "b".repeat(64),
};
const scriptPath = join(
  dirname(fileURLToPath(import.meta.url)),
  "sync-blueprint-from.mjs",
);

// The generated profile module as prettier leaves it: a profile name that is an
// identifier is unquoted, and the selection wraps onto a second line.
const profilesModule = (selected, digestOf = digests) =>
  `export const DEPLOYMENT_PROFILE_DIGESTS = {\n` +
  `  mainnet: "${"c".repeat(64)}",\n` +
  Object.entries(digestOf)
    .map(([name, digest]) => `  "${name}":\n    "${digest}",\n`)
    .join("") +
  `} as const;\n\n` +
  `export const SELECTED_DEPLOYMENT_PROFILE =\n  DEPLOYMENT_PROFILES[${JSON.stringify(selected)}];\n`;

const writeTree = (
  root,
  { compiler = pin, selected = "preprod-testing" } = {},
) => {
  const write = (path, contents) => {
    mkdirSync(dirname(join(root, path)), { recursive: true });
    writeFileSync(join(root, path), contents);
  };
  for (const workflow of ["aiken-ci.yml", "midgard-node-ci.yml"]) {
    write(
      `.github/workflows/${workflow}`,
      `env:\n  AIKEN_FORK_VERSION: ${compiler}\n`,
    );
  }
  write("onchain/aiken/aiken.toml", 'name = "midgard/selftest"\n');
  write("onchain/aiken/aiken.lock", "# lock\n");
  write("onchain/aiken/lib/midgard/probe.ak", "pub const x = 1\n");
  write("onchain/aiken/validators/probe.ak", "validator probe { }\n");
  write("onchain/aiken/env/default.ak", "pub const network = 0\n");
  write(
    "demo/midgard-core/src/generated-deployment-profiles.ts",
    profilesModule(selected),
  );
  return write;
};

// Writes a blueprint and the build record `deployment-profiles.mjs build`
// would write for it, so the stamp in `root` is fresh.
const buildIn = (
  root,
  blueprint = '{"validators":["built"]}\n',
  profile = "preprod-testing",
) => {
  const blueprintPath = join(root, "onchain/aiken/plutus.json");
  writeFileSync(blueprintPath, blueprint);
  writeFileSync(
    buildRecordPath(blueprintPath),
    JSON.stringify({
      profile: { name: profile },
      profileDigest: digests[profile],
      blueprintHash: blueprintHash(blueprintPath),
      sourceHash: blueprintSourceHash(root),
      compiler: pin,
    }),
  );
  return blueprintPath;
};

const withTrees = (
  run,
  { destinationCompiler, sourceProfile, copyFile } = {},
) => {
  const scratch = mkdtempSync(join(tmpdir(), "sync-blueprint-from-"));
  try {
    const source = join(scratch, "source");
    const destination = join(scratch, "destination");
    const writeSource = writeTree(source);
    const writeDestination = writeTree(destination, {
      ...(destinationCompiler === undefined
        ? {}
        : { compiler: destinationCompiler }),
    });
    const sourceBlueprint = buildIn(source, undefined, sourceProfile);
    const output = { stdout: "", stderr: "" };
    const sync = (...args) =>
      main(args, {
        root: destination,
        stdout: (text) => {
          output.stdout += text;
        },
        stderr: (text) => {
          output.stderr += text;
        },
        ...(copyFile === undefined ? {} : { copyFile }),
      });
    run({
      source,
      destination,
      sourceBlueprint,
      destinationBlueprint: join(destination, "onchain/aiken/plutus.json"),
      writeSource,
      writeDestination,
      sync,
      output,
    });
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
};

test("copies a fresh blueprint whose inputs match and leaves it fresh here", () => {
  withTrees(
    ({
      source,
      sourceBlueprint,
      destinationBlueprint,
      destination,
      sync,
      output,
    }) => {
      assert.equal(sync(source), EXIT.copied, output.stderr);
      assert.deepEqual(
        readFileSync(destinationBlueprint),
        readFileSync(sourceBlueprint),
      );
      assert.deepEqual(
        readFileSync(buildRecordPath(destinationBlueprint)),
        readFileSync(buildRecordPath(sourceBlueprint)),
      );
      assert.equal(checkBlueprintStamp({ root: destination }).status, "fresh");
      assert.match(output.stdout, /copied from/u);
    },
  );
});

test("replaces a stale blueprint already in the destination", () => {
  withTrees(({ source, destination, destinationBlueprint, sync, output }) => {
    buildIn(destination, '{"validators":["old"]}\n');
    writeFileSync(destinationBlueprint, '{"validators":["edited"]}\n');
    assert.equal(checkBlueprintStamp({ root: destination }).status, "stale");
    assert.equal(sync(source), EXIT.copied, output.stderr);
    assert.equal(checkBlueprintStamp({ root: destination }).status, "fresh");
  });
});

test("--dry-run says it would copy and writes nothing", () => {
  withTrees(({ source, destinationBlueprint, sync, output }) => {
    assert.equal(sync(source, "--dry-run"), EXIT.copied, output.stderr);
    assert.match(output.stdout, /would copy/u);
    assert.equal(existsSync(destinationBlueprint), false);
    assert.equal(existsSync(buildRecordPath(destinationBlueprint)), false);
  });
});

test("refuses when an input differs, naming the first differing file", () => {
  withTrees(
    ({ source, destinationBlueprint, writeDestination, sync, output }) => {
      writeDestination(
        "onchain/aiken/validators/probe.ak",
        "validator probe { changed }\n",
      );
      writeDestination(
        "onchain/aiken/lib/midgard/probe.ak",
        "pub const x = 2\n",
      );
      assert.equal(sync(source), EXIT.refused);
      assert.match(
        output.stderr,
        /First differing input: onchain\/aiken\/lib\/midgard\/probe\.ak: contents differ/u,
      );
      assert.equal(existsSync(destinationBlueprint), false);
    },
  );
});

test("refuses when an input exists in only one tree", () => {
  withTrees(({ source, writeDestination, sync, output }) => {
    writeDestination("onchain/aiken/env/extra.ak", "pub const y = 1\n");
    assert.equal(sync(source), EXIT.refused);
    assert.match(
      output.stderr,
      /onchain\/aiken\/env\/extra\.ak: exists here, not in the source/u,
    );
  });
});

test("refuses a source whose own stamp is stale, and copies nothing", () => {
  withTrees(({ source, destinationBlueprint, writeSource, sync, output }) => {
    writeSource("onchain/aiken/lib/midgard/probe.ak", "pub const x = 3\n");
    assert.equal(sync(source), EXIT.refused);
    assert.match(output.stderr, /source blueprint is not fresh/u);
    assert.equal(existsSync(destinationBlueprint), false);
  });
});

test("refuses a source with no blueprint", () => {
  withTrees(({ source, sourceBlueprint, sync, output }) => {
    rmSync(sourceBlueprint);
    assert.equal(sync(source), EXIT.refused);
    assert.match(output.stderr, /no blueprint at/u);
  });
});

test("refuses when the two trees pin different compilers", () => {
  withTrees(
    ({ source, sync, output }) => {
      assert.equal(sync(source), EXIT.refused);
      assert.match(output.stderr, /AIKEN_FORK_VERSION/u);
    },
    { destinationCompiler: "aiken v9.9.9+other" },
  );
});

test("refuses a source built for another profile, naming both", () => {
  withTrees(
    ({ source, destinationBlueprint, sync, output }) => {
      assert.equal(sync(source, "--dry-run"), EXIT.refused);
      assert.match(
        output.stderr,
        /built for profile 'local-devnet-testing', and this checkout selects 'preprod-testing'/u,
      );
      assert.equal(sync(source), EXIT.refused);
      assert.equal(existsSync(destinationBlueprint), false);
    },
    { sourceProfile: "local-devnet-testing" },
  );
});

test("refuses when the selected profile's digest differs here", () => {
  withTrees(
    ({ source, destinationBlueprint, writeDestination, sync, output }) => {
      writeDestination(
        "demo/midgard-core/src/generated-deployment-profiles.ts",
        profilesModule("preprod-testing", {
          ...digests,
          "preprod-testing": "d".repeat(64),
        }),
      );
      assert.equal(sync(source), EXIT.refused);
      assert.match(output.stderr, /profile 'preprod-testing' differs/u);
      assert.equal(existsSync(destinationBlueprint), false);
    },
  );
});

test("a source that changes during the copy replaces nothing here", () => {
  withTrees(
    ({ source, destination, destinationBlueprint, sync, output }) => {
      buildIn(destination, '{"validators":["mine"]}\n');
      const before = readFileSync(destinationBlueprint);
      const beforeRecord = readFileSync(buildRecordPath(destinationBlueprint));
      assert.equal(sync(source), EXIT.refused);
      assert.match(output.stderr, /nothing was replaced/u);
      assert.deepEqual(readFileSync(destinationBlueprint), before);
      assert.deepEqual(
        readFileSync(buildRecordPath(destinationBlueprint)),
        beforeRecord,
      );
      assert.deepEqual(
        readdirSync(join(destination, "onchain/aiken")).filter((name) =>
          name.includes(".tmp"),
        ),
        [],
      );
    },
    {
      // Stands in for a rebuild in the source between the checks and the copy.
      copyFile: (from, to) => {
        copyFileSync(from, to);
        if (from.endsWith("plutus.json")) {
          writeFileSync(to, '{"validators":["rebuilt meanwhile"]}\n');
        }
      },
    },
  );
});

// Runs a sync whose copy of the build record is rewritten by `rewriteRecord`,
// and checks that the destination's own pair survives and nothing is staged.
const syncWithStagedRecord = (rewriteRecord, check) =>
  withTrees(
    ({ source, destination, destinationBlueprint, sync, output }) => {
      buildIn(destination, '{"validators":["mine"]}\n');
      const before = readFileSync(destinationBlueprint);
      const beforeRecord = readFileSync(buildRecordPath(destinationBlueprint));
      check({ code: sync(source), output });
      assert.deepEqual(readFileSync(destinationBlueprint), before);
      assert.deepEqual(
        readFileSync(buildRecordPath(destinationBlueprint)),
        beforeRecord,
      );
      assert.deepEqual(
        readdirSync(join(destination, "onchain/aiken")).filter((name) =>
          name.includes(".tmp"),
        ),
        [],
      );
    },
    {
      copyFile: (from, to) => {
        copyFileSync(from, to);
        if (to.endsWith(".deployment.json")) rewriteRecord(to);
      },
    },
  );

test("a source rebuilt for another profile during the copy replaces nothing here", () => {
  // The staged pair stays fresh (blueprint and source hashes still match), so
  // only the second profile check can refuse it.
  syncWithStagedRecord(
    (to) => {
      const record = JSON.parse(readFileSync(to, "utf8"));
      writeFileSync(
        to,
        JSON.stringify({
          ...record,
          profile: { name: "local-devnet-testing" },
          profileDigest: digests["local-devnet-testing"],
        }),
      );
    },
    ({ code, output }) => {
      assert.equal(code, EXIT.refused);
      assert.match(
        output.stderr,
        /nothing was replaced: the source was built for profile 'local-devnet-testing'/u,
      );
    },
  );
});

test("a staged build record that cannot be read exits 2 and replaces nothing", () => {
  syncWithStagedRecord(
    (to) => writeFileSync(to, "{not json"),
    ({ code, output }) => {
      assert.equal(code, EXIT.usage);
      assert.match(output.stderr, /could not judge/u);
    },
  );
});

test("a copy that throws removes what it staged and replaces nothing", () => {
  withTrees(
    ({ source, destination, destinationBlueprint, sync }) => {
      buildIn(destination, '{"validators":["mine"]}\n');
      const before = readFileSync(destinationBlueprint);
      assert.throws(() => sync(source), /disk full/u);
      assert.deepEqual(readFileSync(destinationBlueprint), before);
      assert.deepEqual(
        readdirSync(join(destination, "onchain/aiken")).filter((name) =>
          name.includes(".tmp"),
        ),
        [],
      );
    },
    {
      // The blueprint copy lands, then the record copy fails.
      copyFile: (from, to) => {
        if (to.endsWith(".deployment.json")) throw new Error("disk full");
        copyFileSync(from, to);
      },
    },
  );
});

test("bad arguments exit 2", () => {
  withTrees(({ source, destination, sync }) => {
    assert.equal(sync(), EXIT.usage);
    assert.equal(sync(source, source), EXIT.usage);
    assert.equal(sync("--force"), EXIT.usage);
    assert.equal(sync(destination), EXIT.usage);
    assert.equal(sync(join(source, "onchain")), EXIT.usage);
  });
});

test("the command line exits 2 on a path that is not a checkout", () => {
  const scratch = mkdtempSync(join(tmpdir(), "sync-blueprint-from-cli-"));
  try {
    const run = spawnSync(process.execPath, [scriptPath, scratch], {
      encoding: "utf8",
    });
    assert.equal(run.status, EXIT.usage);
    assert.match(run.stderr, /has no onchain\/aiken/u);
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
});
