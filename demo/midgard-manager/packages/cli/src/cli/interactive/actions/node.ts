import { confirm, input } from '@inquirer/prompts';
import chalk from 'chalk';
import { Effect } from 'effect';
import ora from 'ora-classic';

import { saveConfig } from '../../../config/index.js';
import { formatError, MidgardError } from '../../../utils/errors.js';
import type { Action } from '../types.js';

/**
 * Interactive action that updates the configured Midgard node endpoint.
 *
 * Persisting the endpoint through a dedicated action keeps the rest of the CLI
 * on a validated configuration path instead of accepting arbitrary strings at
 * each call site.
 */
export const configureNodeEndpoint: Action = {
  name: 'Configure Node Endpoint',
  description: 'Set the Midgard node endpoint URL',
  execute: async (context) => {
    try {
      console.log(chalk.dim('Press Ctrl+C to cancel this operation and return to menu\n'));

      // Create a local abort controller that can be linked to the parent
      const abortController = new AbortController();

      // Listen for the parent abort signal to propagate it
      /**
       * Aborts the active prompt when the enclosing interactive session is
       * interrupted.
       */
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
              } catch {
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
            endpoint,
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
      const errorName = error instanceof Error ? error.name : undefined;
      const errorMessage = formatError(error);
      if (errorName === 'AbortError' || errorMessage === 'AbortPromptError') {
        const abortError = new Error('Operation cancelled');
        abortError.name = 'AbortPromptError';
        throw abortError;
      }
      throw MidgardError.config(`Failed to update node endpoint: ${errorMessage}`);
    }
  },
};
