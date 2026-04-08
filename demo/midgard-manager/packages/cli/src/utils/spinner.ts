import ora from 'ora-classic';

/**
 * Creates the shared CLI spinner configuration.
 *
 * Keeping this wrapper centralized avoids diverging spinner styles across the
 * interactive manager.
 */
export function createSpinner(message: string) {
  return ora({
    text: message,
    spinner: 'dots', // Uses a nice clean looking spinner
    color: 'green',
  });
}
