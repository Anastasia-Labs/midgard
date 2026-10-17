import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
import {
  existsSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";
import {
  buildInvocation,
  isolateCodexMcp,
  resolveSeat,
  runInvocation,
  validateConfig,
} from "./dispatch.mjs";

const config = JSON.parse(
  readFileSync(new URL("../../../pstack-models.json", import.meta.url), "utf8"),
);
const cli = fileURLToPath(new URL("./dispatch.mjs", import.meta.url));
const cloned = () => structuredClone(config);
const nodeStub =
  (script, observe = () => {}) =>
  (command, args, options) => {
    observe(command, args, options);
    return spawn(process.execPath, ["-e", script], options);
  };

test("shipped roles resolve native workers and both provider panel seats", () => {
  assert.equal(validateConfig(config), config);
  assert.deepEqual(resolveSeat(config, "feature, refactoring", 0, "claude"), {
    provider: "claude",
    model: undefined,
  });
  assert.equal(
    resolveSeat(config, "interrogate reviewers", 0, "claude").provider,
    "codex",
  );
  assert.equal(
    resolveSeat(config, "interrogate reviewers", 1, "codex").provider,
    "claude",
  );
});

test("invalid configuration fails before any provider execution", () => {
  const mutations = [
    (c) => {
      c.roles["unknown role"] = { provider: "native" };
    },
    (c) => {
      delete c.roles["bug-fix"];
    },
    (c) => {
      c.roles["arena runners"] = [];
    },
    (c) => {
      c.roles["bug-fix"] = { provider: "unavailable" };
    },
    (c) => {
      c.roles["bug-fix"] = { provider: "codex", model: "--bypass" };
    },
    (c) => {
      c.roles["bug-fix"] = { provider: "claude", model: "auto" };
    },
    (c) => {
      c.roles["bug-fix"] = { provider: "native", effort: "unlimited" };
    },
    (c) => {
      c.roles["bug-fix"] = { provider: "native", shell: true };
    },
  ];
  for (const mutate of mutations) {
    const value = cloned();
    mutate(value);
    assert.throws(() => validateConfig(value));
  }
  assert.throws(
    () => resolveSeat(config, "bug-fix", 1, "codex"),
    /does not exist/u,
  );
  assert.throws(() => resolveSeat(config, "bug-fix", 0, "other"), /Host/u);
});

test("model IDs and effort stay separate, with explicit native aliases", () => {
  const value = cloned();
  value.roles["bug-fix"] = {
    provider: "native",
    model: "inherit-parent",
    effort: "high",
  };
  assert.deepEqual(resolveSeat(value, "bug-fix", 0, "codex"), {
    provider: "codex",
    model: undefined,
    effort: "high",
  });
  const invocation = buildInvocation(
    { provider: "codex", model: "gpt-example", effort: "high" },
    "/tmp",
  );
  assert.ok(invocation.args.includes("gpt-example"));
  assert.ok(invocation.args.includes('model_reasoning_effort="high"'));
  const other = buildInvocation(
    { provider: "claude", model: "opus", effort: "high" },
    "/tmp",
  );
  assert.deepEqual(other.args.slice(-4), [
    "--model",
    "opus",
    "--effort",
    "high",
  ]);
});

test("provider task receives literal stdin without shell interpolation", async () => {
  const prompt =
    "Review `symbol` and $(touch /tmp/should-not-exist)\nKeep all lines.\n";
  for (const provider of ["codex", "claude"]) {
    const invocation = buildInvocation({ provider }, tmpdir());
    const result = await runInvocation(invocation, prompt, {
      spawnImpl: nodeStub(
        "process.stdin.pipe(process.stdout)",
        (command, args, options) => {
          assert.equal(command, provider);
          assert.equal(options.shell, false);
          assert.equal(options.cwd, tmpdir());
          if (provider === "codex") {
            assert.ok(args.includes("read-only"));
            assert.ok(args.includes('approval_policy="never"'));
            assert.ok(args.includes("hooks"));
          } else {
            assert.ok(args.includes("dontAsk"));
            assert.ok(args.includes("Read,Glob,Grep"));
            assert.ok(args.includes("--strict-mcp-config"));
          }
        },
      ),
    });
    assert.equal(result.output, prompt);
  }
});

test("provider failure, timeout, excessive output and empty success cannot become verdicts", async () => {
  const invocation = buildInvocation({ provider: "codex" }, tmpdir());
  await assert.rejects(
    runInvocation(invocation, "review", {
      spawnImpl: nodeStub("process.stdin.resume(); process.exitCode=3"),
    }),
    /exited 3/u,
  );
  await assert.rejects(
    runInvocation(invocation, "review", {
      spawnImpl: nodeStub("process.stdin.resume();"),
    }),
    /empty response/u,
  );
  await assert.rejects(
    runInvocation(invocation, "review", {
      timeoutMs: 80,
      spawnImpl: nodeStub("process.stdin.resume(); setInterval(()=>{},1000)"),
    }),
    /timed out/u,
  );
  await assert.rejects(
    runInvocation(invocation, "review", {
      maxBytes: 10,
      spawnImpl: nodeStub(
        'process.stdin.resume(); process.stdout.write("x".repeat(100))',
      ),
    }),
    /exceeded limit/u,
  );
  await assert.rejects(
    runInvocation(
      { command: "/nonexistent/pstack-provider", args: [], cwd: tmpdir() },
      "review",
    ),
    /ENOENT/u,
  );
});

test("CLI dry-run prints dispatch metadata without starting a provider or exposing prompt", (t) => {
  const dir = mkdtempSync(join(tmpdir(), "pstack-dispatch-"));
  t.after(() => rmSync(dir, { recursive: true }));
  const input = join(dir, "prompt.txt"),
    output = join(dir, "verdict.txt"),
    models = join(dir, "models.json");
  writeFileSync(input, "private review text");
  writeFileSync(models, JSON.stringify(config));
  const args = [
    cli,
    "--config",
    models,
    "--workspace",
    dir,
    "--role",
    "interrogate reviewers",
    "--seat",
    "1",
    "--host",
    "codex",
    "--prompt",
    input,
    "--output",
    output,
    "--dry-run",
  ];
  const result = spawnSync(process.execPath, args, {
    encoding: "utf8",
    env: { ...process.env, PATH: "" },
  });
  assert.equal(result.status, 0, result.stderr);
  const parsed = JSON.parse(result.stdout);
  assert.equal(parsed.command, "claude");
  assert.equal(parsed.cwd, dir);
  assert.equal(result.stdout.includes("private review text"), false);
  assert.equal(existsSync(output), false);
  writeFileSync(output, "existing verdict");
  const rejected = spawnSync(process.execPath, args, { encoding: "utf8" });
  assert.equal(rejected.status, 1);
  assert.match(rejected.stderr, /Output already exists/u);
  assert.equal(readFileSync(output, "utf8"), "existing verdict");
});

test("Codex review disables every effective MCP server and verifies the override before launch", () => {
  const invocation = buildInvocation({ provider: "codex" }, tmpdir());
  let calls = 0;
  const isolated = isolateCodexMcp(invocation, (command, args, options) => {
    assert.equal(command, "codex");
    assert.equal(options.cwd, tmpdir());
    assert.equal(options.timeout, 15000);
    assert.ok(args.includes("plugins"));
    if (calls++ === 0)
      return JSON.stringify([
        { name: "existing", enabled: true },
        { name: "already-disabled", enabled: false },
      ]);
    assert.ok(args.includes("mcp_servers.existing.enabled=false"));
    assert.ok(args.includes("mcp_servers.already-disabled.enabled=false"));
    return JSON.stringify([{ name: "existing", enabled: false }]);
  });
  assert.equal(calls, 2);
  assert.ok(isolated.args.includes("mcp_servers.existing.enabled=false"));
  assert.equal(isolated.args.at(-1), "-");
  assert.throws(
    () =>
      isolateCodexMcp(invocation, () =>
        JSON.stringify([{ name: "retained", enabled: true }]),
      ),
    /isolation failed/u,
  );
  assert.throws(
    () => isolateCodexMcp(invocation, () => "invalid JSON"),
    /enumerate/u,
  );
  assert.throws(
    () =>
      isolateCodexMcp(invocation, () =>
        JSON.stringify([{ name: "missing-enabled-field" }]),
      ),
    /Invalid/u,
  );
  const claude = buildInvocation({ provider: "claude" }, tmpdir());
  assert.equal(
    isolateCodexMcp(claude, () => {
      throw new Error("must not execute");
    }),
    claude,
  );
});

test("unrepresentable MCP names fail without guessing a dotted configuration key", () => {
  const invocation = buildInvocation({ provider: "codex" }, tmpdir());
  assert.throws(
    () =>
      isolateCodexMcp(invocation, () =>
        JSON.stringify([{ name: "server.with.dot", enabled: true }]),
      ),
    /cannot be safely overridden/u,
  );
});
