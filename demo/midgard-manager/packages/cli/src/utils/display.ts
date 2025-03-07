import { getGeneratorStatus } from '@midgard-manager/tx-generator';
import { MidgardNodeClient } from '@midgard-manager/tx-generator';
import chalk from 'chalk';

import type { MidgardConfig } from '../types/config.js';
import { displayLogo } from './logo.js';

/**
 * Clears the terminal screen and displays a header
 * @param title Optional title to display in the header
 */
export function displayHeader(title?: string) {
  // Clear the terminal screen
  process.stdout.write('\x1Bc');

  // Display the Midgard logo with the title if provided
  displayLogo({
    variant: 'small',
    headerText: title,
  });
}

/**
 * Displays status information in a minimalist, left-aligned format
 */
export async function displayStatus(config: MidgardConfig) {
  // Get the actual running status of the transaction generator
  const generatorStatus = getGeneratorStatus();
  const isRunning = generatorStatus.running;

  // Check if the node is available with a very short timeout
  const nodeClient = new MidgardNodeClient({
    baseUrl: config.node.endpoint,
  });

  // Check node availability with shorter timeout
  let nodeConnected = false;
  try {
    // Use a quick timeout of 500ms for better UI responsiveness
    const timeoutMs = 500;
    nodeConnected = await nodeClient.isAvailable(timeoutMs);
  } catch (error) {
    // On errors, assume disconnected
    nodeConnected = false;
  }

  // Minimalist left-aligned border style
  console.log('\n┌─ NODE');

  // Show node status with connection indicator
  const nodeStatusSymbol = nodeConnected ? chalk.green('●') : chalk.red('●');
  const nodeStatusText = nodeConnected ? 'Connected' : 'Disconnected';

  console.log(`│  ${nodeStatusSymbol} ${nodeStatusText}`);
  console.log(`│  ${config.node.endpoint}`);
  console.log('│');
  console.log('├─ TRANSACTION GENERATOR');

  // Status with simple color indicators
  const statusSymbol = isRunning ? chalk.green('●') : chalk.red('●');
  const statusText = isRunning ? 'Running' : 'Not Running';

  console.log(`│  ${statusSymbol} ${statusText}`);

  if (config.generator.enabled) {
    console.log('│');
    console.log(`│  ${chalk.dim('Default Configuration')}`);
    console.log(
      `│  Concurrency: ${config.generator.maxConcurrent}   Batch: ${config.generator.batchSize}`
    );
    console.log(`│  Interval: ${config.generator.intervalMs}ms`);

    // Only show statistics if running
    if (isRunning) {
      console.log('│');
      console.log(`│  ${chalk.dim('Transactions')}`);
      console.log(`│  Generated: ${generatorStatus.transactionsGenerated}`);
      console.log(`│  Submitted: ${generatorStatus.transactionsSubmitted}`);
    }
  } else {
    console.log('│  Configure options from the menu to get started');
  }

  // Version footer
  console.log('│');
  console.log(`└─ Version ${process.env.npm_package_version || '0.1.0'}`);
  console.log('');
}

/**
 * Displays keyboard navigation hints in a minimalist format
 */
export function displayKeyboardHints() {
  console.log('┌─ CONTROLS');
  console.log(
    `│  ${chalk.bold('↑/↓')}: Navigate   ${chalk.bold(
      'Enter'
    )}: Select   ${chalk.bold('Ctrl+C')}: Cancel/Exit`
  );
  console.log('└────────────');
}

/**
 * Displays a success message with simple styling
 */
export function displaySuccess(message: string) {
  console.log(`\n${chalk.green('✓')} ${chalk.bold(message)}`);
  console.log('─────────────');
}

/**
 * Displays an error message with stable styling
 */
export function displayError(message: string | Error, details?: unknown) {
  const errorMessage = typeof message === 'string' ? message : message.message;

  console.log('\n┌─ ERROR');
  console.log(`│  ${chalk.bold(errorMessage)}`);

  if (details) {
    const detailsStr = String(details);
    console.log(`│  ${detailsStr}`);
  }

  // Add recovery hint when possible
  if (typeof message === 'string') {
    if (message.includes('endpoint')) {
      console.log(`│  ${chalk.dim('Check network or node status')}`);
    } else if (message.includes('config')) {
      console.log(`│  ${chalk.dim('Check config file permissions')}`);
    }
  }

  console.log('└────────────');
}

/**
 * Displays a "Press Enter to continue" message
 */
export function displayContinuePrompt() {
  console.log(`\nPress ${chalk.bold('Enter')} to continue...`);
}
