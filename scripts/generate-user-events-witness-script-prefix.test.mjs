import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  copyFileSync,
  cpSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

// A bare `aiken fmt` formats the whole Aiken project, which rewrites about 250
// unrelated .ak files and mixes them into whatever work the tree holds. The
// generator must format only the environments it writes. This test copies it
// into a throwaway project, built by the real aiken from demo/flake.lock, with
// a stub profile generator that writes unformatted environments, and checks
// that those are formatted while an unformatted library module is left alone.

const here = dirname(fileURLToPath(import.meta.url));
const root = join(here, "..");
const scriptSource = join(
  here,
  "generate-user-events-witness-script-prefix.sh",
);

const mockRaw = "77".repeat(32);

const witness = `use cardano/certificate.{Certificate}
use cardano/transaction.{Transaction}

validator main(nonce: ByteArray) {
  publish(_redeemer: Data, _certificate: Certificate, _self: Transaction) {
    nonce != #""
  }

  else(_) {
    fail
  }
}
`;
const unformattedLib = "pub const  y = 2\n";

// Stands in for `deployment-profiles.mjs generate`: writes two environments.
const stubGenerator = `import { writeFileSync } from "node:fs";
for (const name of ["default", "testnet"]) {
  writeFileSync(new URL(\`../../onchain/aiken/env/\${name}.ak\`, import.meta.url), "pub const  x = 1\\n");
}
`;

const withTree = (body) => {
  const scratch = realpathSync(
    mkdtempSync(join(tmpdir(), "witness-prefix-fmt-scope-")),
  );
  try {
    const repo = join(scratch, "repo");
    const write = (path, contents) => {
      mkdirSync(dirname(join(repo, path)), { recursive: true });
      writeFileSync(join(repo, path), contents);
    };
    write(
      "config/deployments/env.ak.template",
      'pub const user_events_witness_script_prefix: ByteArray =\n  #"00"\n',
    );
    write("demo/scripts/deployment-profiles.mjs", stubGenerator);
    for (const path of [
      "demo/flake.lock",
      "scripts/aiken",
      "scripts/lib/aiken.mjs",
      "onchain/aiken/aiken.toml",
      "onchain/aiken/aiken.lock",
    ]) {
      mkdirSync(dirname(join(repo, path)), { recursive: true });
      cpSync(join(root, path), join(repo, path));
    }
    // The checkout's fetched dependencies, when present, save a download.
    if (existsSync(join(root, "onchain/aiken/build/packages")))
      cpSync(
        join(root, "onchain/aiken/build/packages"),
        join(repo, "onchain/aiken/build/packages"),
        { recursive: true },
      );
    write("onchain/aiken/validators/user-events/witness.ak", witness);
    write("onchain/aiken/lib/midgard/untouched.ak", unformattedLib);
    for (const name of ["default", "testnet"])
      write(`onchain/aiken/env/${name}.ak`, "pub const x = 1\n");
    copyFileSync(
      scriptSource,
      join(repo, "scripts/generate-user-events-witness-script-prefix.sh"),
    );
    const run = () =>
      spawnSync(
        "bash",
        [join(repo, "scripts/generate-user-events-witness-script-prefix.sh")],
        { cwd: repo, encoding: "utf8" },
      );
    body({ repo, run });
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
};

test("writes the prefix and formats only the generated environments", () => {
  withTree(({ repo, run }) => {
    const result = run();
    assert.equal(result.status, 0, result.stderr);
    const template = readFileSync(
      join(repo, "config/deployments/env.ak.template"),
      "utf8",
    );
    const [, prefix] =
      /^pub const user_events_witness_script_prefix: ByteArray =\n  #"([0-9a-f]+)"\n$/u.exec(
        template,
      ) ?? [];
    assert.ok(prefix, template);
    assert.ok(prefix.endsWith("5820"), prefix);
    assert.ok(!prefix.includes(mockRaw));
    for (const name of ["default", "testnet"])
      assert.equal(
        readFileSync(join(repo, `onchain/aiken/env/${name}.ak`), "utf8"),
        "pub const x = 1\n",
      );
    assert.equal(
      readFileSync(
        join(repo, "onchain/aiken/lib/midgard/untouched.ak"),
        "utf8",
      ),
      unformattedLib,
    );
  });
});
