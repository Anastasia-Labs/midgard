/**
 * Waits for one raw keypress before resuming normal stdin handling.
 *
 * `Ctrl+C` is treated as an immediate process exit so the prompt behaves like a
 * normal terminal application instead of trapping the interrupt.
 */
export function waitForKeypress(): Promise<void> {
  return new Promise((resolve) => {
    process.stdin.setRawMode(true);
    process.stdin.resume();
    process.stdin.once('data', (data) => {
      // Ctrl+C should exit the process
      if (data.toString() === '\u0003') {
        process.exit(0);
      }
      process.stdin.setRawMode(false);
      process.stdin.pause();
      resolve();
    });
  });
}
