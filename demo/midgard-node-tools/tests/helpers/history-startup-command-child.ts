import { runHistoryRoleCommand } from "../../src/devnet-stack/history-role-command.js";

const root = process.argv[2];
if (root === undefined) throw new Error("owned command root absent");
const originalNow = performance.now.bind(performance);
let calls = 0;
let elapsed = 0;
Object.defineProperty(performance, "now", {
  value: () => {
    calls += 1;
    // Advance a controlled monotonic clock after the first absolute cutoff exists.
    if (calls === 2) {
      elapsed = 6000;
      console.log(
        JSON.stringify({ firstAttemptExpired: true, pid: process.pid }),
      );
    }
    return originalNow() + elapsed;
  },
});
await runHistoryRoleCommand({ runDir: root, role: "archive", provider: "a" });
console.log(JSON.stringify({ commandJoined: true, pid: process.pid }));
