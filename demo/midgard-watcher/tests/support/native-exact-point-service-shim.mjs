// The persistent exact-point helper for per-process executable fixtures:
// every framed session runs the fixture script itself as one ordinary helper
// process (same startup line, stdout, stderr and exit), so fixtures that
// assert per-query process lifetimes keep observing one process per session.
// A closed session is terminated like the owner terminated a helper process,
// and its end frame follows only after that process has exited.
import { spawn } from "node:child_process";
import { createInterface } from "node:readline";

const script = process.argv[1];
const sessions = new Map();
const frame = (text) => process.stdout.write(text);
const reader = createInterface({ input: process.stdin, crlfDelay: Infinity });
let lastId = 0;

reader.on("line", (request) => {
  const [verb, rawId] = request.split(" ", 2);
  const id = Number(rawId);
  if (!/^[1-9][0-9]{0,15}$/u.test(rawId ?? "")) process.exit(65);
  if (verb === "close") {
    if (id > lastId) process.exit(65);
    const child = sessions.get(rawId);
    if (child !== undefined && child.exitCode === null) child.kill("SIGTERM");
    return;
  }
  if (verb !== "open" || id <= lastId) process.exit(65);
  lastId = id;
  const child = spawn(process.execPath, [script], {
    stdio: ["pipe", "pipe", "pipe"],
    env: process.env,
  });
  sessions.set(rawId, child);
  const stdout = createInterface({ input: child.stdout, crlfDelay: Infinity });
  stdout.on("line", (line) => frame(`out ${rawId} ${line}\n`));
  child.stderr.on("data", (chunk) =>
    frame(`err ${rawId} ${Buffer.from(chunk).toString("base64")}\n`),
  );
  child.stdin.on("error", () => undefined);
  child.stdin.end(`${request.slice(verb.length + rawId.length + 2)}\n`);
  let stdoutClosed = false;
  let exitStatus;
  const finish = () => {
    if (!stdoutClosed || exitStatus === undefined) return;
    sessions.delete(rawId);
    frame(`end ${rawId} ${exitStatus}\n`);
  };
  stdout.once("close", () => {
    stdoutClosed = true;
    finish();
  });
  child.once("exit", (code, signal) => {
    exitStatus = code ?? (signal === null ? 1 : 128);
    finish();
  });
});

reader.once("close", async () => {
  const live = [...sessions.values()];
  for (const child of live) child.kill("SIGTERM");
  await Promise.all(
    live.map(
      (child) =>
        new Promise((resolve) => {
          if (child.exitCode !== null || child.signalCode !== null) resolve();
          else child.once("exit", resolve);
        }),
    ),
  );
  process.exit(0);
});

await new Promise(() => undefined);
