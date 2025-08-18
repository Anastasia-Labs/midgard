import { Command } from '@effect/cli';
import { select } from '@inquirer/prompts';
import chalk from 'chalk';
import { Effect, pipe } from 'effect';
import ora from 'ora-classic';

import { loadConfig } from '../../config/index.js';
import type { MidgardConfig } from '../../types/config.js';
import {
  displayContinuePrompt,
  displayError,
  displayHeader,
  displayKeyboardHints,
  displayStatus,
  displaySuccess,
} from '../../utils/display.js';
import { waitForKeypress } from '../../utils/input.js';
import { sleep } from '../../utils/sleep.js';
import { menu } from './menu.js';

const BACK_OPTION = {
  name: `${chalk.dim('←')} ${chalk.green('Back to main menu')}`,
  value: '__back',
  description: '',
};

export const interactiveCommand = Command.make('interactive', {}, () => {
  return pipe(
    Effect.tryPromise(async () => {
      const spinner = ora({
        text: 'Loading configuration...',
        color: 'green',
      }).start();
      try {
        const config = await Effect.runPromise(loadConfig);
        spinner.succeed('Configuration loaded');
        const manualCommandImpl = new ManualCommandImpl(config);

        // Ensure proper cleanup on process exit
        process.once('SIGINT', () => {
          manualCommandImpl.cleanup();
          console.log('\n\nExiting Midgard CLI...\n');
          process.exit(0);
        });

        await sleep(500); // Brief pause for UX
        displayHeader('Interactive Mode');
        await manualCommandImpl.loop();

        // Clean up resources if we exit the loop normally
        manualCommandImpl.cleanup();
      } catch (error) {
        spinner.fail('Failed to load configuration');
        throw error;
      }
    }),
    Effect.catchAll((error) => {
      displayError('Error starting interactive mode', error);
      return Effect.succeed(0);
    })
  );
});

class ManualCommandImpl {
  private _config: MidgardConfig;
  private _abortController: AbortController;
  private _isPromptActive: boolean = false;
  private _sigintHandler: (() => void) | null = null;

  constructor(config: MidgardConfig) {
    this._config = config;
    this._abortController = new AbortController();

    // Set up signal handler to prevent Ctrl+C from killing the process
    this._sigintHandler = () => {
      // Only abort if prompt is active, otherwise let the normal exit happen
      if (this._isPromptActive) {
        console.log('\n'); // Add a new line for better appearance
        this._abortController.abort();
        // Create a new abort controller for the next prompt
        this._abortController = new AbortController();
      } else {
        // If no prompt is active, exit normally with a goodbye message
        console.log('\n\nExiting Midgard CLI...\n');
        process.exit(0);
      }
    };

    process.on('SIGINT', this._sigintHandler);
  }

  /**
   * Clean up resources when the command is done
   */
  cleanup(): void {
    // Remove our SIGINT handler to prevent memory leaks
    if (this._sigintHandler) {
      process.off('SIGINT', this._sigintHandler);
      this._sigintHandler = null;
    }

    // Cancel any active prompts
    if (this._abortController) {
      try {
        this._abortController.abort();
      } catch (e) {
        // Ignore errors on cleanup
      }
    }
  }

  async loop() {
    while (true) {
      try {
        displayHeader('Main Menu');
        await displayStatus(this._config);
        displayKeyboardHints();

        this._isPromptActive = true;
        const section = await select(
          {
            message: chalk.bold.green('Select a section:'),
            choices: menu.sections.map((section) => ({
              name: `${chalk.white.bold(section.name)} ${chalk.dim('- ' + section.description)}`,
              value: section.name,
            })),
          },
          { signal: this._abortController.signal }
        );
        this._isPromptActive = false;

        const sectionMenu = menu.sections.find((s) => s.name === section);
        if (!sectionMenu) continue;

        while (true) {
          displayHeader(`${sectionMenu.name}`);
          displayKeyboardHints();

          this._isPromptActive = true;
          const action = await select(
            {
              message: chalk.bold.green(`Select an action:`),
              choices: [
                BACK_OPTION,
                ...sectionMenu.actions.map((action) => ({
                  name: `${chalk.white.bold(action.name)} ${chalk.dim('- ' + action.description)}`,
                  value: action.name,
                })),
              ],
            },
            { signal: this._abortController.signal }
          );
          this._isPromptActive = false;

          // If user selected back, break the inner loop to return to section selection
          if (action === '__back') break;

          const selectedAction = sectionMenu.actions.find((a) => a.name === action);
          if (!selectedAction) continue;

          displayHeader(`${sectionMenu.name} > ${selectedAction.name}`);

          try {
            // Execute the action without an initial spinner
            // This allows the action to show its own UI elements like prompts
            this._isPromptActive = true;
            const result = await selectedAction.execute({
              config: this._config,
            });
            this._isPromptActive = false;

            // Only show spinner for config refresh
            const configSpinner = ora({
              text: 'Refreshing configuration...',
              color: 'green',
            }).start();
            this._config = await Effect.runPromise(loadConfig);
            configSpinner.succeed('Configuration updated');

            displaySuccess(result.message);
            displayContinuePrompt();
            // Wait for user to press a key before continuing
            await waitForKeypress();
          } catch (error) {
            this._isPromptActive = false;
            if (error instanceof Error && error.name === 'AbortPromptError') {
              // Show a clearer message when a prompt is cancelled
              console.log(`\n\n${chalk.green('✓ Action cancelled')} - Returning to menu\n`);
              // Wait a moment before returning to the menu for better UX
              await sleep(500);
            } else {
              displayError('Error executing action', error);
            }
            displayContinuePrompt();
            // Wait for user to press a key before continuing
            await waitForKeypress();
          }
        }
      } catch (error) {
        this._isPromptActive = false;
        if (error instanceof Error && error.name === 'AbortPromptError') {
          // Show a clearer message when a prompt is cancelled
          console.log(`\n\n${chalk.green('✓ Action cancelled')} - Returning to menu\n`);
          // Wait a moment before returning to the menu for better UX
          await sleep(500);
        } else {
          displayError('Error in command loop', error);
        }
      }
    }
  }
}
