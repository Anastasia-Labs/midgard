/**
 * Waits for user to press any key
 * @returns A promise that resolves when any key is pressed
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
