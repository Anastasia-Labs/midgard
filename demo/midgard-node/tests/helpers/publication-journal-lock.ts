import { spawn } from "node:child_process";
import { once } from "node:events";

/**
 * Linux devnet journal lock. The kernel owns exclusion; there is no stale-PID
 * unlink window. The pipe closes on owner death, releasing the helper's flock.
 * Keep the lock inode permanently: unlinking a locked inode breaks exclusion.
 */
export const acquirePublicationJournalLock = async (
  path: string,
): Promise<() => Promise<void>> => {
  const child = spawn(
    "flock",
    [
      "--exclusive",
      "--nonblock",
      path,
      process.execPath,
      "--input-type=module",
      "-e",
      "process.stdout.write('locked\\n'); process.stdin.resume(); await new Promise(resolve => process.stdin.once('end', resolve));",
    ],
    { stdio: ["pipe", "pipe", "pipe"] },
  );
  let diagnostic = "";
  child.stderr.on("data", (chunk: Buffer) => {
    diagnostic += chunk.toString();
  });
  await new Promise<void>((resolve, reject) => {
    child.once("error", reject);
    child.once("exit", (code) =>
      reject(
        new Error(
          code === 1
            ? "Publication journal is locked by another process"
            : `Publication journal flock failed (${code}): ${diagnostic}`,
        ),
      ),
    );
    child.stdout.once("data", (chunk: Buffer) => {
      if (chunk.toString() === "locked\n") resolve();
      else reject(new Error("Publication journal lock handshake differs"));
    });
  }).catch((cause) => {
    child.stdin.destroy();
    throw cause;
  });
  return async () => {
    if (child.exitCode !== null)
      throw new Error("Publication journal lost its kernel lock");
    const exited = once(child, "exit");
    child.stdin.end();
    const [code] = await exited;
    if (code !== 0)
      throw new Error(`Publication journal lock release failed (${code})`);
  };
};
