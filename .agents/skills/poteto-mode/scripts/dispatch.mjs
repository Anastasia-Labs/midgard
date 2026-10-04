import { execFileSync, spawn } from "node:child_process";
import { lstatSync, readFileSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { parseArgs } from "node:util";

export const ROLE_NAMES = [
  "feature, refactoring",
  "bug-fix",
  "perf-issue",
  "hillclimb",
  "judgment and prose",
  "hardest tasks",
  "how explorer",
  "how explainer",
  "why investigators",
  "why synthesizer",
  "reflect tooling",
  "reflect judgment, divergent, synthesizer",
  "swarm workers",
  "arena runners",
  "arena cross-judge pool",
  "architect runners",
  "interrogate reviewers",
];
const PANELS = new Set(ROLE_NAMES.slice(-4));
const PROVIDERS = new Set(["native", "codex", "claude"]);
const EFFORTS = new Set(["low", "medium", "high", "xhigh", "max"]);
const ALIASES = new Set(["auto", "inherit-parent"]);
const object = (value) =>
  value !== null && typeof value === "object" && !Array.isArray(value);

export function validateConfig(config) {
  if (
    !object(config) ||
    Object.keys(config).join() !== "roles" ||
    !object(config.roles)
  ) {
    throw new Error("Configuration must contain only a roles object");
  }
  const unknown = Object.keys(config.roles).filter(
    (role) => !ROLE_NAMES.includes(role),
  );
  if (unknown.length)
    throw new Error(`Unknown pstack roles: ${unknown.join(", ")}`);
  for (const role of ROLE_NAMES) {
    const entry = config.roles[role];
    const panel = PANELS.has(role);
    if (panel ? !Array.isArray(entry) || !entry.length : !object(entry)) {
      throw new Error(
        `${role}: expected ${panel ? "a nonempty panel" : "one seat"}`,
      );
    }
    for (const seat of panel ? entry : [entry]) {
      if (!object(seat) || !PROVIDERS.has(seat.provider)) {
        throw new Error(`${role}: provider must be native, codex, or claude`);
      }
      if (
        Object.keys(seat).some(
          (key) => !["provider", "model", "effort"].includes(key),
        )
      ) {
        throw new Error(`${role}: unknown seat field`);
      }
      if (
        seat.model !== undefined &&
        (typeof seat.model !== "string" ||
          !/^[A-Za-z0-9][A-Za-z0-9._:/-]*$/u.test(seat.model))
      ) {
        throw new Error(`${role}: invalid model ID`);
      }
      if (ALIASES.has(seat.model) && seat.provider !== "native") {
        throw new Error(`${role}: ${seat.model} only applies to native seats`);
      }
      if (seat.effort !== undefined && !EFFORTS.has(seat.effort)) {
        throw new Error(`${role}: unsupported effort`);
      }
    }
  }
  return config;
}

export function resolveSeat(config, role, index, host) {
  validateConfig(config);
  if (!ROLE_NAMES.includes(role)) throw new Error(`Unknown role: ${role}`);
  if (!["codex", "claude"].includes(host))
    throw new Error("Host must be codex or claude");
  const entry = config.roles[role];
  const seats = Array.isArray(entry) ? entry : [entry];
  if (!Number.isSafeInteger(index) || index < 0 || index >= seats.length) {
    throw new Error(`Seat ${index} does not exist for ${role}`);
  }
  const seat = seats[index];
  return {
    ...seat,
    provider: seat.provider === "native" ? host : seat.provider,
    model: ALIASES.has(seat.model) ? undefined : seat.model,
  };
}

export function buildInvocation(seat, workspace) {
  const cwd = resolve(workspace);
  if (!["codex", "claude"].includes(seat.provider))
    throw new Error("Resolve native seats first");
  let args;
  if (seat.provider === "codex") {
    args = [
      "exec",
      "--ephemeral",
      "--sandbox",
      "read-only",
      "--cd",
      cwd,
      "--config",
      'approval_policy="never"',
      "--config",
      "--config",
      'web_search="disabled"',
      "--disable",
      "hooks",
      "--disable",
      "plugins",
      "--disable",
      "apps",
      "--disable",
      "multi_agent",
      "--disable",
      "multi_agent_v2",
      "--disable",
      "browser_use",
      "--disable",
      "computer_use",
      "--disable",
      "image_generation",
    ];
    if (seat.model !== undefined) args.push("--model", seat.model);
    if (seat.effort !== undefined) {
      args.push(
        "--config",
        `model_reasoning_effort=${JSON.stringify(seat.effort)}`,
      );
    }
    args.push("-");
  } else {
    args = [
      "--print",
      "--output-format",
      "text",
      "--no-session-persistence",
      "--permission-mode",
      "dontAsk",
      "--tools",
      "Read,Glob,Grep",
      "--allowedTools",
      "Read,Glob,Grep",
      "--strict-mcp-config",
      "--mcp-config",
      "{}",
      "--settings",
      '{"disableAllHooks":true}',
    ];
    if (seat.model !== undefined) args.push("--model", seat.model);
    if (seat.effort !== undefined) args.push("--effort", seat.effort);
  }
  return { command: seat.provider, args, cwd };
}

export function isolateCodexMcp(invocation, execImpl = execFileSync) {
  if (invocation.command !== "codex") return invocation;
  const baseArgs = [
    "--disable",
    "hooks",
    "--disable",
    "plugins",
    "--disable",
    "apps",
  ];
  const list = (overrides) => {
    let servers;
    try {
      servers = JSON.parse(
        execImpl(
          "codex",
          [...baseArgs, ...overrides, "mcp", "list", "--json"],
          {
            cwd: invocation.cwd,
            encoding: "utf8",
            timeout: 15_000,
            maxBuffer: 8 * 1024 * 1024,
            stdio: ["ignore", "pipe", "pipe"],
          },
        ),
      );
    } catch {
      throw new Error(
        "Could not enumerate Codex MCP configuration; no review started",
      );
    }
    if (
      !Array.isArray(servers) ||
      servers.some(
        (server) =>
          !object(server) ||
          typeof server.name !== "string" ||
          !server.name ||
          typeof server.enabled !== "boolean",
      )
    ) {
      throw new Error("Invalid Codex MCP inventory; no review started");
    }
    return servers;
  };
  const servers = list([]);
  if (servers.some(({ name }) => !/^[A-Za-z0-9_-]+$/u.test(name))) {
    throw new Error(
      "MCP server name cannot be safely overridden by this Codex CLI; no review started",
    );
  }
  const overrides = servers.flatMap(({ name }) => [
    "--config",
    `mcp_servers.${name}.enabled=false`,
  ]);
  if (list(overrides).some(({ enabled }) => enabled)) {
    throw new Error("Codex MCP isolation failed; no review started");
  }
  return {
    ...invocation,
    args: [...invocation.args.slice(0, -1), ...overrides, "-"],
  };
}

export function runInvocation(
  invocation,
  prompt,
  { timeoutMs = 600_000, maxBytes = 8 * 1024 * 1024, spawnImpl = spawn } = {},
) {
  return new Promise((accept, reject) => {
    const child = spawnImpl(invocation.command, invocation.args, {
      cwd: invocation.cwd,
      shell: false,
      stdio: ["pipe", "pipe", "pipe"],
      // A separate Claude process is intentional, rather than a recursive CLI session.
      env: Object.fromEntries(
        Object.entries(process.env).filter(([key]) => key !== "CLAUDECODE"),
      ),
    });
    const stdout = [];
    const stderr = [];
    let bytes = 0;
    let failure;
    const stop = (reason) => {
      failure ??= new Error(reason);
      child.kill("SIGKILL");
    };
    const timer = setTimeout(
      () => stop(`Provider timed out after ${timeoutMs} ms`),
      timeoutMs,
    );
    const collect = (destination) => (chunk) => {
      bytes += chunk.length;
      if (bytes > maxBytes) stop("Provider output exceeded limit");
      else destination.push(chunk);
    };
    child.stdout.on("data", collect(stdout));
    child.stderr.on("data", collect(stderr));
    child.on("error", (error) => {
      clearTimeout(timer);
      reject(error);
    });
    child.stdin.on("error", (error) => {
      failure ??= error;
    });
    child.on("close", (code, signal) => {
      clearTimeout(timer);
      const output = Buffer.concat(stdout).toString("utf8");
      if (failure) reject(failure);
      else if (code !== 0)
        reject(
          new Error(`Provider exited ${code ?? signal}; no verdict produced`),
        );
      else if (!output.trim())
        reject(new Error("Provider returned an empty response"));
      else accept({ output, stderr: Buffer.concat(stderr).toString("utf8") });
    });
    child.stdin.end(prompt);
  });
}

const USAGE = `Usage: node dispatch.mjs --check-config <file>
  or: node dispatch.mjs --role <name> --host codex|claude --prompt <file>
      --workspace <repo-root> --output <new-file> [--seat 0] [--config <file>]
      [--timeout-ms 600000] [--dry-run]`;

export async function main(argv) {
  const { values } = parseArgs({
    args: argv,
    options: {
      "check-config": { type: "string" },
      config: { type: "string" },
      role: { type: "string" },
      host: { type: "string" },
      seat: { type: "string", default: "0" },
      workspace: { type: "string" },
      prompt: { type: "string" },
      output: { type: "string" },
      "timeout-ms": { type: "string", default: "600000" },
      "dry-run": { type: "boolean" },
      help: { type: "boolean" },
    },
  });
  if (values.help) {
    console.log(USAGE);
    return;
  }
  if (values["check-config"]) {
    validateConfig(JSON.parse(readFileSync(values["check-config"], "utf8")));
    console.log("pstack model configuration is valid");
    return;
  }
  for (const key of ["role", "host", "workspace", "prompt", "output"]) {
    if (!values[key]) throw new Error(`Missing --${key}\n${USAGE}`);
  }
  const workspace = resolve(values.workspace);
  if (!lstatSync(workspace).isDirectory())
    throw new Error("Workspace must be a directory");
  const config = JSON.parse(
    readFileSync(
      values.config ?? resolve(workspace, ".agents/pstack-models.json"),
      "utf8",
    ),
  );
  const index = /^\d+$/u.test(values.seat) ? Number(values.seat) : NaN;
  const seat = resolveSeat(config, values.role, index, values.host);
  const invocation = buildInvocation(seat, workspace);
  const timeoutMs = Number(values["timeout-ms"]);
  if (
    !Number.isSafeInteger(timeoutMs) ||
    timeoutMs < 1 ||
    timeoutMs > 3_600_000
  ) {
    throw new Error("Timeout must be an integer between 1 and 3600000 ms");
  }
  const outputPath = resolve(values.output);
  if (lstatSync(outputPath, { throwIfNoEntry: false }))
    throw new Error("Output already exists; choose a new file");
  const prompt = readFileSync(values.prompt, "utf8");
  if (!prompt.trim()) throw new Error("Prompt is empty");
  if (values["dry-run"]) {
    console.log(
      JSON.stringify({ ...invocation, output: outputPath, timeoutMs }, null, 2),
    );
    return;
  }
  const result = await runInvocation(
    isolateCodexMcp(invocation),
    "Read-only pstack task. Follow applicable repository instructions. Return findings or a design as text. " +
      "Do not edit files, run external actions, or delegate. State evidence and gaps.\n\n" +
      prompt,
    { timeoutMs },
  );
  writeFileSync(outputPath, result.output, { flag: "wx", mode: 0o600 });
  console.log(
    `pstack ${seat.provider} (${seat.model ?? "runtime default"}): result saved to ${outputPath}`,
  );
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  main(process.argv.slice(2)).catch((error) => {
    console.error(error.message);
    process.exitCode = 1;
  });
}
