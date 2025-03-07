import chalk from 'chalk';

/**
 * Logo display options
 */
export interface LogoOptions {
  /** Show header text after the logo */
  headerText?: string;
}

/**
 * Displays the Midgard logo
 */
export function displayLogo(options: LogoOptions = {}): void {
  const { headerText } = options;
  const treeEmoji = '🌳';

  console.log(`┌─ ${treeEmoji} ${chalk.bold.green('MIDGARD MANAGER')} `);
  console.log(`│ ${chalk.dim('MVP')} · CLI Tool for Testing/Demo`);
  console.log('└────────────');

  // Show header text if provided
  if (headerText) {
    console.log(`\n${chalk.bold(headerText)}\n`);
  }
}
