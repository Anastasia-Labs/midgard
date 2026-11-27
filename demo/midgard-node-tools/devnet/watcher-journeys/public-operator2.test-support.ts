import { execFile } from "node:child_process";
import {
  chmod,
  mkdir,
  mkdtemp,
  readFile,
  rm,
  writeFile,
} from "node:fs/promises";
import { type AddressInfo, createServer as createNetServer } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { promisify } from "node:util";

import type { StackTls } from "./attached-journey-session.test-support.js";

const run = promisify(execFile);
const PUBLIC_SERVICES = ["midgard-node", "public-ingress"] as const;
const composedPidFiles: string[] = [];
const roots: string[] = [];
/**
 * Stops every process a scripted Compose project left running and removes
 * its directory; for `afterEach`.
 */
export const stopComposedServices = async () => {
  for (const pidFile of composedPidFiles.splice(0)) {
    const pid = Number(await readFile(pidFile, "utf8").catch(() => "0"));
    if (pid > 0)
      try {
        process.kill(pid);
      } catch {
        // Already stopped.
      }
  }
  await Promise.all(
    roots.splice(0).map((path) => rm(path, { recursive: true, force: true })),
  );
};

const freePort = () =>
  new Promise<number>((resolve, reject) => {
    const server = createNetServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const { port } = server.address() as AddressInfo;
      server.close(() => resolve(port));
    });
  });

/**
 * Operator-2's instance of a public stack, as `docker compose -p <project>`
 * runs it: a `docker` CLI for the test's `PATH` that runs the project's
 * `midgard-node` and `public-ingress` services as real processes, the way
 * the stack's generated Compose file defines them (`public-runtime.ts`).
 * The node serves plain HTTP on a Unix socket only, from 2 s after its
 * container starts. The ingress serves the node's API over HTTPS
 * on the instance's loopback port; while the node's socket does not serve,
 * it answers every request 503 with the ingress's own JSON
 * (`midgard-node/scripts/public-ingress.mjs`). Each service stops and starts
 * on its own: stopping `midgard-node` leaves the ingress serving. The CLI
 * logs every invocation and refuses any other verb, any other project, and
 * any `COMPOSE_*` variable or working directory that could load another
 * project's files.
 */
export const composedPublicOperator2 = async (tls: StackTls) => {
  const root = await mkdtemp(join(tmpdir(), "op2-public-"));
  roots.push(root);
  const bin = join(root, "bin");
  await mkdir(bin);
  const port = await freePort();
  const composeProject = "midgard-node-op2";
  const log = join(root, "docker.log");
  const socket = join(root, "node.sock");
  const answers = join(root, "ingress.log");
  const pidFile = (service: string) => join(root, `${service}.pid`);
  for (const service of PUBLIC_SERVICES)
    composedPidFiles.push(pidFile(service));
  const services = {
    "midgard-node": `const { createServer } = require("node:http");
const { rmSync } = require("node:fs");
process.stdout.write("up");
setTimeout(() => {
  rmSync(${JSON.stringify(socket)}, { force: true });
  createServer((request, response) => {
    response.writeHead(request.url === "/readyz" ? 200 : 404, { "content-type": "application/json" });
    response.end(JSON.stringify(request.url === "/readyz" ? { ready: true, reasons: [] } : {}));
  }).listen(${JSON.stringify(socket)});
}, 2000);`,
    "public-ingress": `const { appendFileSync, readFileSync } = require("node:fs");
const { request } = require("node:http");
const { createServer } = require("node:https");
const unavailable = JSON.stringify({ error: "The node is unavailable; retry later", errorCode: "E_HTTP_INGRESS_CAPACITY" });
createServer({ cert: readFileSync(${JSON.stringify(tls.certFile)}), key: readFileSync(${JSON.stringify(tls.keyFile)}) }, (incoming, response) => {
  const upstream = request({ socketPath: ${JSON.stringify(socket)}, path: incoming.url, method: incoming.method }, (answer) => {
    appendFileSync(${JSON.stringify(answers)}, answer.statusCode + "\\n");
    response.writeHead(answer.statusCode, { "content-type": "application/json" });
    answer.pipe(response);
  });
  upstream.on("error", () => {
    appendFileSync(${JSON.stringify(answers)}, "503 unavailable\\n");
    response.writeHead(503, { "content-type": "application/json", "retry-after": "5" });
    response.end(unavailable);
  });
  upstream.end();
}).listen(${port.toString()}, "127.0.0.1", () => process.stdout.write("up"));`,
  };
  await writeFile(
    join(bin, "docker"),
    `#!${process.execPath}
const { spawn } = require("node:child_process");
const { appendFileSync, existsSync, readFileSync, rmSync, writeFileSync } = require("node:fs");
const args = process.argv.slice(2);
appendFileSync(${JSON.stringify(log)}, JSON.stringify(args) + "\\n");
if (Object.keys(process.env).some((name) => name.startsWith("COMPOSE_")) || process.cwd() !== "/") {
  console.error("compose would load another project's files");
  process.exit(125);
}
const [compose, p, project, verb, ...rest] = args;
if (compose !== "compose" || p !== "-p") process.exit(125);
if (project !== ${JSON.stringify(composeProject)}) {
  console.error("no such project " + project);
  process.exit(1);
}
const services = ${JSON.stringify(services)};
const pidFile = (service) => ${JSON.stringify(root)} + "/" + service + ".pid";
const alive = (service) => {
  if (!existsSync(pidFile(service))) return false;
  try { process.kill(Number(readFileSync(pidFile(service), "utf8")), 0); return true; } catch { return false; }
};
const named = rest.length > 0 && rest.every((service) => service in services);
const main = async () => {
  if (verb === "ps" && rest.join(" ") === "--status running --services") {
    console.log("postgres");
    for (const service of Object.keys(services)) if (alive(service)) console.log(service);
  } else if (verb === "stop" && named) {
    for (const service of rest) {
      if (alive(service)) {
        const pid = Number(readFileSync(pidFile(service), "utf8"));
        process.kill(pid);
        for (let i = 0; i < 200; i += 1) {
          try { process.kill(pid, 0); } catch { break; }
          Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 10);
        }
      }
      rmSync(pidFile(service), { force: true });
    }
  } else if (verb === "start" && named) {
    for (const service of rest) {
      if (alive(service)) continue;
      const child = spawn(process.execPath, ["-e", services[service]], { detached: true, stdio: ["ignore", "pipe", "ignore"] });
      await new Promise((resolve) => child.stdout.once("data", resolve));
      writeFileSync(pidFile(service), String(child.pid));
      child.unref();
    }
  } else {
    console.error("unsupported compose verb " + JSON.stringify(rest.length === 0 ? verb : [verb, ...rest]));
    process.exit(125);
  }
  process.exit(0);
};
main();
`,
  );
  await chmod(join(bin, "docker"), 0o755);
  const path = `${bin}:${process.env.PATH ?? ""}`;
  return {
    composeProject,
    /** The instance's node API, as its ingress serves it on loopback. */
    endpoint: `https://127.0.0.1:${port.toString()}`,
    /** A `PATH` whose `docker` is this project's. */
    path,
    /** Starts both services, as the stack started the instance. */
    up: async () => {
      await run(
        "docker",
        ["compose", "-p", composeProject, "start", ...PUBLIC_SERVICES],
        { cwd: "/", env: { PATH: path } },
      );
    },
    /** The ingress's answers in order: the node's status, or its own 503. */
    ingressAnswers: async () =>
      (await readFile(answers, "utf8").catch(() => ""))
        .split("\n")
        .filter((line) => line !== ""),
    /** Each `docker compose -p <project>` invocation's verb and arguments. */
    invocations: async () =>
      (await readFile(log, "utf8").catch(() => ""))
        .trim()
        .split("\n")
        .filter((line) => line !== "")
        .map((line) => (JSON.parse(line) as string[]).slice(3).join(" ")),
  };
};
