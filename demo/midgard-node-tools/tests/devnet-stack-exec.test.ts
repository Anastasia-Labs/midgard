import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import { execLogged } from "../src/devnet-stack/exec.js";

const dirs: string[] = [];
const pids: number[] = [];
afterEach(() => {
  for (const pid of pids.splice(0))
    try {
      process.kill(pid, "SIGKILL");
    } catch {
      // Already gone.
    }
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const logDir = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-exec-"));
  dirs.push(dir);
  return dir;
};

/** Ignores SIGTERM, like a CLI whose finalizers never finish. */
const STUBBORN =
  "process.on('SIGTERM', () => {}); console.log('up'); setInterval(() => {}, 1000)";

describe("execLogged", () => {
  it("escalates a timed-out child that ignores SIGTERM to SIGKILL", async () => {
    const result = await execLogged(process.execPath, ["-e", STUBBORN], {
      logDir: logDir(),
      label: "stubborn",
      timeoutMs: 2_000,
      killGraceMs: 500,
    });
    expect(result).toMatchObject({ code: null, signal: "SIGKILL" });
    expect(readFileSync(result.log, "utf8")).toMatch(
      /signal=SIGKILL timed out after 2 s/,
    );
  }, 10_000);

  it("settles even when a grandchild keeps the output pipes open", async () => {
    // The shell execs the stubborn child; the background sleep inherits its
    // stdout and survives the child's SIGKILL.
    const result = await execLogged(
      "sh",
      [
        "-c",
        `sleep 30 & echo "grandchild $!"; exec "${process.execPath}" -e "${STUBBORN}"`,
      ],
      {
        logDir: logDir(),
        label: "grandchild",
        timeoutMs: 2_000,
        killGraceMs: 500,
      },
    );
    const grandchild = Number(/grandchild (\d+)/u.exec(result.stdout)?.[1]);
    pids.push(grandchild);
    expect(result.signal).toBe("SIGKILL");
    // The pipes were still held when the result settled.
    expect(() => process.kill(grandchild, 0)).not.toThrow();
  }, 10_000);

  it("reports an ordinary exit without a timeout note", async () => {
    const result = await execLogged(
      process.execPath,
      ["-e", "process.exit(3)"],
      {
        logDir: logDir(),
        label: "exit",
        timeoutMs: 10_000,
      },
    );
    expect(result).toMatchObject({ code: 3, signal: null });
    expect(readFileSync(result.log, "utf8")).toMatch(
      /\[exit code=3 signal=null\]\n$/u,
    );
  });
});
