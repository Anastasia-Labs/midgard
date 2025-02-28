import { confirm, input } from '@inquirer/prompts';
import chalk from 'chalk';
import { Effect } from 'effect';
import ora from 'ora-classic';

import { saveConfig } from '../../../config/index.js';
import { MidgardError } from '../../../utils/errors.js';
import type { Action } from '../types.js';

export const configureNodeEndpoint: Action = {
  name: 'Configure Node Endpoint',
  description: 'Set the Midgard node endpoint URL',
  execute: async (context) => {
    try {
      console.log(chalk.dim('Press Ctrl+C to cancel this operation and return to menu\n'));

      // Create a local abort controller that can be linked to the parent
      const abortController = new AbortController();

      // Listen for the parent abort signal to propagate it
      const parentAbortHandler = () => {
        abortController.abort();
        throw new Error('AbortPromptError');
      };

      // Add a listener for SIGINT that will abort our local controller
      process.once('SIGINT', parentAbortHandler);

      try {
        // Prompt for the endpoint URL
        const endpoint = await input(
          {
            message: 'Enter Node endpoint URL:',
            default: context.config.node.endpoint,
            validate: (value: string) => {
              try {
                new URL(value);
                return true;
              } catch (e) {
                return 'Please enter a valid URL (e.g. http://localhost:3000)';
              }
            },
          },
          { signal: abortController.signal }
        );

        // Confirm before saving
        const confirmed = await confirm(
          {
            message: `Save endpoint as ${endpoint}?`,
            default: true,
          },
          { signal: abortController.signal }
        );

        if (!confirmed) {
          return {
            success: true,
            message: 'Node configuration cancelled',
          };
        }

        const saveSpinner = ora('Saving node configuration...').start();

        const newConfig = {
          ...context.config,
          node: {
            ...context.config.node,
            endpoint: endpoint,
          },
        };

        await Effect.runPromise(saveConfig(newConfig));

        saveSpinner.succeed('Node configuration saved');

        return {
          success: true,
          message: 'Node endpoint updated successfully',
        };
      } finally {
        // Clean up our SIGINT handler
        process.off('SIGINT', parentAbortHandler);
      }
    } catch (error) {
      // Check if it's an abort error and rethrow with the expected name
      if (error.name === 'AbortError' || error.message === 'AbortPromptError') {
        const abortError = new Error('Operation cancelled');
        abortError.name = 'AbortPromptError';
        throw abortError;
      }
      throw MidgardError.config(`Failed to update node endpoint: ${error}`);
    }
  },
};

export const clearNodeDatabase: Action = {
  name: 'Clear Mempool (Not Implemented)',
  description: "Placeholder for clearing the node's mempool (not yet available)",
  execute: async (context) => {
    try {
      console.log(chalk.dim('Press Ctrl+C to cancel this operation and return to menu\n'));

      // Create a local abort controller
      const abortController = new AbortController();

      // Listen for the parent abort signal to propagate it
      const parentAbortHandler = () => {
        abortController.abort();
        throw new Error('AbortPromptError');
      };

      // Add a listener for SIGINT that will abort our local controller
      process.once('SIGINT', parentAbortHandler);

      try {
        console.log(chalk.yellow('⚠️  Note: This feature is not yet fully implemented.'));
        console.log(
          chalk.yellow(
            '   Currently there is no actual node connection or mempool clearing functionality.'
          )
        );
        console.log('');

        // More accurate confirmation for mempool clearing
        const firstConfirm = await confirm(
          {
            message: 'Do you want to continue with this placeholder action?',
            default: false,
          },
          { signal: abortController.signal }
        );

        if (!firstConfirm) {
          return {
            success: true,
            message: 'Operation cancelled',
          };
        }

        const clearSpinner = ora('Simulating mempool clearing...').start();

        // TODO: Implement actual mempool clear logic
        // This would likely call node APIs to clear the mempool and mempoolLedger tables
        await new Promise((resolve) => setTimeout(resolve, 1500));

        clearSpinner.succeed('Operation completed (placeholder only)');
        console.log(
          chalk.dim('💡 Note: No actual mempool was cleared as this feature is not yet implemented')
        );

        return {
          success: true,
          message: 'Feature placeholder executed successfully',
        };
      } finally {
        // Clean up our SIGINT handler
        process.off('SIGINT', parentAbortHandler);
      }
    } catch (error) {
      // Check if it's an abort error and rethrow with the expected name
      if (error.name === 'AbortError' || error.message === 'AbortPromptError') {
        const abortError = new Error('Operation cancelled');
        abortError.name = 'AbortPromptError';
        throw abortError;
      }
      throw MidgardError.node(`Error in placeholder function: ${error}`);
    }
  },
};
