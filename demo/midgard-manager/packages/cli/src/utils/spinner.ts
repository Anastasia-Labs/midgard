import ora from 'ora-classic';

/**
 * Creates and returns a spinner with the given message
 * @param message Message to display with the spinner
 * @returns The ora spinner instance
 */
export function createSpinner(message: string) {
  return ora({
    text: message,
    spinner: 'dots', // Uses a nice clean looking spinner
    color: 'green',
  });
}
