import { Args, Command, Options } from '@effect/cli';
import { startGenerator, stopGenerator } from '@midgard-manager/tx-generator';
import chalk from 'chalk';
import { Effect, pipe } from 'effect';
import fs from 'fs/promises';
import cron from 'node-cron';
import ora from 'ora-classic';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

import { getWallet, listWallets } from '../../config/wallets.js';

/**
 * Scheduler commands for recurring transaction-generation workloads.
 *
 * This module owns both persisted schedule configuration and the in-memory
 * CRON jobs derived from that configuration.
 */

/**
 * Resolve the manager project root from the compiled command location so the
 * schedule store is stable regardless of the caller's working directory.
 */
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const MONOREPO_ROOT = join(__dirname, '../../../../../../..');
const PROJECT_ROOT = join(MONOREPO_ROOT, 'demo/midgard-manager');

/**
 * Schedule configuration lives alongside the rest of the manager config files.
 */
const CONFIG_DIR = join(PROJECT_ROOT, 'config');
const SCHEDULE_CONFIG_PATH = join(CONFIG_DIR, 'schedules.json');

/**
 * Transaction types a scheduled job may request from the generator.
 */
const transactionTypes = ['one-to-one', 'multi-output', 'mixed'];

/**
 * Human-friendly CRON presets offered by the CLI.
 */
const schedulePresets = {
  hourly: '0 * * * *',
  daily: '0 0 * * *',
  weekday: '0 9 * * 1-5',
  weekend: '0 10 * * 0,6',
  'every-minute': '* * * * *', // For testing
  'every-5-minutes': '*/5 * * * *',
  'every-30-minutes': '*/30 * * * *',
};

/**
 * Serialized schedule-file shape.
 */
interface ScheduleConfig {
  schedules: Schedule[];
}

/**
 * One persisted recurring transaction-generation schedule.
 */
interface Schedule {
  id: string;
  name: string;
  cronExpression: string;
  enabled: boolean;
  transactionConfig: {
    transactionType: 'one-to-one' | 'multi-output' | 'mixed';
    oneToOneRatio?: number;
    batchSize: number;
    interval: number;
    concurrency: number;
    nodeEndpoint: string;
    wallet: string;
  };
}

/**
 * In-memory registry of active CRON jobs keyed by schedule id.
 */
const activeJobs: Map<string, cron.ScheduledTask> = new Map();

/**
 * Loads the persisted schedule list from disk, creating an empty file on first
 * use when needed.
 */
const loadSchedules = async (): Promise<ScheduleConfig> => {
  try {
    // Ensure directory exists
    await fs.mkdir(CONFIG_DIR, { recursive: true });

    try {
      const data = await fs.readFile(SCHEDULE_CONFIG_PATH, 'utf-8');
      return JSON.parse(data);
    } catch (error) {
      // If file doesn't exist, create empty config
      const emptyConfig: ScheduleConfig = { schedules: [] };
      await fs.writeFile(SCHEDULE_CONFIG_PATH, JSON.stringify(emptyConfig, null, 2));
      return emptyConfig;
    }
  } catch (error) {
    console.error(chalk.red(`Error loading schedules: ${error}`));
    return { schedules: [] };
  }
};

/**
 * Persists the full schedule list back to disk.
 */
const saveSchedules = async (config: ScheduleConfig): Promise<void> => {
  try {
    await fs.mkdir(CONFIG_DIR, { recursive: true });
    await fs.writeFile(SCHEDULE_CONFIG_PATH, JSON.stringify(config, null, 2));
  } catch (error) {
    console.error(chalk.red(`Error saving schedules: ${error}`));
    throw error;
  }
};

/**
 * Generates a lightweight identifier for a schedule entry.
 */
const generateId = (): string => {
  return Math.random().toString(36).substring(2, 10);
};

/**
 * Starts or restarts one schedule in the in-memory CRON registry.
 *
 * The function validates the CRON expression, resolves the configured wallet,
 * and ensures only one active task exists per schedule id.
 */
const startScheduledJob = async (schedule: Schedule): Promise<void> => {
  if (activeJobs.has(schedule.id)) {
    // If job is already running, stop it first
    activeJobs.get(schedule.id)?.stop();
    activeJobs.delete(schedule.id);
  }

  if (!schedule.enabled) {
    return; // Don't start disabled schedules
  }

  // Validate the CRON expression
  if (!cron.validate(schedule.cronExpression)) {
    console.error(chalk.red(`Invalid CRON expression: ${schedule.cronExpression}`));
    return;
  }

  // Get the wallet private key
  const wallet = await getWallet(schedule.transactionConfig.wallet);
  if (!wallet) {
    console.error(chalk.red(`Wallet '${schedule.transactionConfig.wallet}' not found`));
    return;
  }

  // Schedule the job
  const task = cron.schedule(schedule.cronExpression, async () => {
    console.log(chalk.blue(`⏰ Running scheduled transaction generation: ${schedule.name}`));

    try {
      await startGenerator({
        transactionType: schedule.transactionConfig.transactionType,
        oneToOneRatio: schedule.transactionConfig.oneToOneRatio || 70,
        batchSize: schedule.transactionConfig.batchSize,
        interval: schedule.transactionConfig.interval,
        concurrency: schedule.transactionConfig.concurrency,
        nodeEndpoint: schedule.transactionConfig.nodeEndpoint,
        walletPrivateKey: wallet.privateKey,

        // Auto-stop after completing a batch for scheduled jobs
        autoStopAfterBatch: true,
      });
    } catch (error) {
      console.error(chalk.red(`Error running scheduled job: ${error}`));
    }
  });

  activeJobs.set(schedule.id, task);
};

/**
 * Restores all enabled schedules from disk during module initialization.
 */
const initSchedules = async (): Promise<void> => {
  const config = await loadSchedules();

  for (const schedule of config.schedules) {
    if (schedule.enabled) {
      await startScheduledJob(schedule);
    }
  }
};

/**
 * CLI command that creates, persists, and activates a new recurring
 * transaction-generation schedule.
 */
const addScheduleCommand = Command.make(
  'add',
  {
    name: Options.text('name').pipe(Options.withDescription('Name for this schedule')),

    preset: Options.choice('preset', Object.keys(schedulePresets))
      .pipe(Options.withDescription('Schedule preset (e.g., hourly, daily)'))
      .pipe(Options.optional),

    cronExpression: Options.text('cron')
      .pipe(
        Options.withDescription("Custom CRON expression (e.g., '0 9 * * 1-5' for weekdays at 9am)")
      )
      .pipe(Options.optional),

    type: Options.choice('type', transactionTypes)
      .pipe(Options.withDescription('Type of transactions to generate'))
      .pipe(Options.withDefault('mixed')),

    batchSize: Options.integer('batch-size')
      .pipe(Options.withDescription('Number of transactions per batch'))
      .pipe(Options.withDefault(10)),

    wallet: Options.text('wallet')
      .pipe(Options.withDescription('Wallet to use for transaction signing'))
      .pipe(Options.withDefault('test')),

    oneToOneRatio: Options.integer('one-to-one-ratio')
      .pipe(
        Options.withDescription("For 'mixed' type: Percentage of one-to-one transactions (0-100)")
      )
      .pipe(Options.withDefault(70)),

    nodeEndpoint: Options.text('node-endpoint')
      .pipe(Options.withDescription('Midgard node endpoint URL'))
      .pipe(Options.withDefault('http://localhost:3000')),

    interactive: Options.boolean('interactive')
      .pipe(Options.withDescription('Use interactive mode for configuration'))
      .pipe(Options.withDefault(false)),
  },
  ({
    name,
    preset,
    cronExpression,
    type,
    batchSize,
    wallet,
    oneToOneRatio,
    nodeEndpoint,
    interactive,
  }) => {
    return pipe(
      Effect.tryPromise(async () => {
        const schedule: Schedule = {
          id: generateId(),
          name: name || 'Unnamed Schedule',
          cronExpression: cronExpression || (preset ? schedulePresets[preset] : '0 * * * *'), // Default to hourly
          enabled: true,
          transactionConfig: {
            transactionType: type as 'one-to-one' | 'multi-output' | 'mixed',
            oneToOneRatio,
            batchSize,
            interval: 5, // Default interval in seconds
            concurrency: 2, // Lower default for scheduled tasks
            nodeEndpoint,
            wallet,
          },
        };

        // Interactive mode
        if (interactive) {
          const { input, select, number, confirm } = await import('@inquirer/prompts');

          // Step 1: Basic schedule info
          schedule.name = await input({
            message: 'Enter a name for this schedule:',
            default: schedule.name,
          });

          // Step 2: Schedule timing
          const timingType = await select({
            message: 'How do you want to set the schedule?',
            choices: [
              { value: 'preset', name: 'Use a preset schedule' },
              { value: 'custom', name: 'Enter a custom CRON expression' },
            ],
          });

          if (timingType === 'preset') {
            const selectedPreset = await select({
              message: 'Select a schedule preset:',
              choices: Object.entries(schedulePresets).map(([key, value]) => ({
                value: key,
                name: `${key} (${value})`,
              })),
            });

            schedule.cronExpression = schedulePresets[selectedPreset];
          } else {
            schedule.cronExpression = await input({
              message: "Enter a CRON expression (e.g., '0 9 * * 1-5' for weekdays at 9am):",
              default: schedule.cronExpression,
              validate: (value) =>
                cron.validate(value) ||
                'Invalid CRON expression. See https://crontab.guru/ for help.',
            });
          }

          // Step 3: Transaction configuration
          schedule.transactionConfig.transactionType = (await select({
            message: 'Select transaction type:',
            choices: [
              { value: 'one-to-one', name: 'One-to-one (single output)' },
              {
                value: 'multi-output',
                name: 'Multi-output (multiple recipients)',
              },
              { value: 'mixed', name: 'Mixed (combination of both types)' },
            ],
            default: schedule.transactionConfig.transactionType,
          })) as 'one-to-one' | 'multi-output' | 'mixed';

          if (schedule.transactionConfig.transactionType === 'mixed') {
            const ratioResult = await number({
              message: 'Percentage of one-to-one transactions (0-100):',
              default: schedule.transactionConfig.oneToOneRatio,
              validate: (value: number | undefined) =>
                (value !== undefined && value >= 0 && value <= 100) ||
                'Must be a percentage between 0 and 100',
            });
            schedule.transactionConfig.oneToOneRatio =
              ratioResult !== undefined ? ratioResult : schedule.transactionConfig.oneToOneRatio;
          }

          const batchSizeResult = await number({
            message: 'Number of transactions per batch:',
            default: schedule.transactionConfig.batchSize,
            validate: (value: number | undefined) =>
              (value !== undefined && value > 0) || 'Must be greater than 0',
          });
          schedule.transactionConfig.batchSize =
            batchSizeResult !== undefined ? batchSizeResult : schedule.transactionConfig.batchSize;

          // Wallet selection
          const wallets = await listWallets();
          if (wallets.length === 0) {
            console.error(chalk.red('❌ No wallets configured. Please add a wallet first.'));
            return;
          }

          schedule.transactionConfig.wallet = await select({
            message: 'Select wallet for transaction signing:',
            choices: wallets.map((w) => ({
              value: w,
              name: `${w}${w === 'test' ? ' (default test wallet)' : ''}`,
            })),
            default: schedule.transactionConfig.wallet,
          });

          // Node endpoint
          schedule.transactionConfig.nodeEndpoint = await input({
            message: 'Midgard node endpoint URL:',
            default: schedule.transactionConfig.nodeEndpoint,
            validate: (value) =>
              /^https?:\/\//.test(value) || 'Must be a valid URL starting with http:// or https://',
          });

          // Confirm configuration
          console.log(chalk.yellow.bold('\nSchedule Summary:'));
          console.log(chalk.yellow(`• Name: ${schedule.name}`));
          console.log(chalk.yellow(`• Schedule: ${schedule.cronExpression}`));
          console.log(
            chalk.yellow(
              `• Next run: ${new Date(
                cron
                  .schedule(schedule.cronExpression, () => {})
                  .nextDate()
                  .valueOf()
              ).toLocaleString()}`
            )
          );
          console.log(
            chalk.yellow(
              `• Transaction Type: ${schedule.transactionConfig.transactionType}${
                schedule.transactionConfig.transactionType === 'mixed'
                  ? ` (${schedule.transactionConfig.oneToOneRatio}% one-to-one)`
                  : ''
              }`
            )
          );
          console.log(
            chalk.yellow(`• Batch Size: ${schedule.transactionConfig.batchSize} transactions`)
          );
          console.log(chalk.yellow(`• Wallet: ${schedule.transactionConfig.wallet}`));
          console.log(chalk.yellow(`• Node: ${schedule.transactionConfig.nodeEndpoint}`));

          const confirmed = await confirm({
            message: 'Create this schedule?',
            default: true,
          });

          if (!confirmed) {
            console.log(chalk.yellow('⚠️ Schedule creation cancelled'));
            return;
          }
        } else {
          // Non-interactive validation
          if (cronExpression) {
            // Validate custom CRON expression if provided
            if (!cron.validate(cronExpression)) {
              console.error(chalk.red(`❌ Invalid CRON expression: ${cronExpression}`));
              console.log(chalk.gray('See https://crontab.guru/ for help with CRON syntax'));
              return;
            }
          } else if (preset) {
            // Check if preset exists and use it
            if (!schedulePresets[preset]) {
              console.error(chalk.red(`❌ Unknown schedule preset: ${preset}`));
              console.log(
                chalk.gray(`Available presets: ${Object.keys(schedulePresets).join(', ')}`)
              );
              return;
            }

            // Apply the preset (already done during initialization, this is just for clarity)
            schedule.cronExpression = schedulePresets[preset];
          }

          // Double-check final CRON expression
          if (!cron.validate(schedule.cronExpression)) {
            console.error(
              chalk.red(`❌ Final CRON expression invalid: ${schedule.cronExpression}`)
            );
            return;
          }

          // Validate oneToOneRatio for mixed type
          if (type === 'mixed' && (oneToOneRatio < 0 || oneToOneRatio > 100)) {
            console.error(chalk.red('❌ one-to-one-ratio must be between 0 and 100'));
            return;
          }

          // Check if wallet exists
          const walletConfig = await getWallet(wallet);
          if (!walletConfig) {
            console.error(chalk.red(`❌ Wallet '${wallet}' not found`));
            return;
          }
        }

        // Save the schedule
        const spinner = ora('Saving transaction schedule...').start();

        try {
          const config = await loadSchedules();
          config.schedules.push(schedule);
          await saveSchedules(config);

          // Start the schedule
          await startScheduledJob(schedule);

          spinner.succeed('Transaction schedule created and activated');

          // Display next run time
          const nextRunDate = new Date(
            cron
              .schedule(schedule.cronExpression, () => {})
              .nextDate()
              .valueOf()
          );
          console.log(
            chalk.green(
              `\n✓ Schedule "${schedule.name}" will next run at: ${nextRunDate.toLocaleString()}`
            )
          );

          // Display helper commands
          console.log(chalk.gray('\nUseful commands:'));
          console.log(chalk.gray(`• List schedules: midgard-manager schedule-tx list`));
          console.log(chalk.gray(`• Enable/disable: midgard-manager schedule-tx toggle <ID>`));
          console.log(chalk.gray(`• Remove: midgard-manager schedule-tx remove <ID>`));
        } catch (error) {
          spinner.fail('Failed to create schedule');
          console.error(chalk.red(`Error: ${error.message}`));
        }
      })
    );
  }
);

/**
 * CLI command that lists all persisted schedules and their next run time.
 */
const listSchedulesCommand = Command.make('list', {}, () => {
  return pipe(
    Effect.tryPromise(async () => {
      const spinner = ora('Loading schedules...').start();

      try {
        const config = await loadSchedules();
        spinner.succeed('Schedules loaded');

        if (config.schedules.length === 0) {
          console.log(chalk.yellow('No transaction schedules configured'));
          console.log(chalk.gray('To create a schedule:'));
          console.log(chalk.gray('$ midgard-manager schedule-tx add --interactive'));
          return;
        }

        console.log(chalk.blue.bold('\n📅 Transaction Schedules\n'));

        config.schedules.forEach((schedule) => {
          const nextRun = new Date(
            cron
              .schedule(schedule.cronExpression, () => {})
              .nextDate()
              .valueOf()
          );

          console.log(
            chalk.white(
              `${
                schedule.enabled ? chalk.green('●') : chalk.gray('○')
              } ${chalk.cyan(schedule.name)} (ID: ${schedule.id})`
            )
          );
          console.log(chalk.gray(`  Schedule: ${schedule.cronExpression}`));
          console.log(
            chalk.gray(`  Next run: ${schedule.enabled ? nextRun.toLocaleString() : 'Disabled'}`)
          );
          console.log(chalk.gray(`  Tx Type: ${schedule.transactionConfig.transactionType}`));
          console.log(chalk.gray(`  Batch Size: ${schedule.transactionConfig.batchSize}`));
          console.log();
        });

        // Helper commands
        console.log(chalk.gray('Commands:'));
        console.log(chalk.gray('• Enable/disable: midgard-manager schedule-tx toggle <ID>'));
        console.log(chalk.gray('• Remove: midgard-manager schedule-tx remove <ID>'));
        console.log(chalk.gray('• Add new: midgard-manager schedule-tx add --interactive'));
      } catch (error) {
        spinner.fail('Failed to load schedules');
        console.error(chalk.red(`Error: ${error.message}`));
      }
    })
  );
});

/**
 * CLI command that toggles one schedule and reconciles the in-memory CRON job
 * registry to match the persisted enabled state.
 */
const toggleScheduleCommand = Command.make(
  'toggle',
  {
    id: Args.text('ID').pipe(Args.withDescription('ID of the schedule to toggle')),
  },
  ({ id }) => {
    return pipe(
      Effect.tryPromise(async () => {
        if (!id) {
          console.error(chalk.red('❌ Schedule ID is required'));
          return;
        }

        const spinner = ora('Updating schedule...').start();

        try {
          const config = await loadSchedules();
          const scheduleIndex = config.schedules.findIndex((s) => s.id === id);

          if (scheduleIndex === -1) {
            spinner.fail(`Schedule with ID '${id}' not found`);
            return;
          }

          // Toggle the enabled status
          config.schedules[scheduleIndex].enabled = !config.schedules[scheduleIndex].enabled;

          // Save the updated config
          await saveSchedules(config);

          // Update the running jobs
          if (config.schedules[scheduleIndex].enabled) {
            await startScheduledJob(config.schedules[scheduleIndex]);
            spinner.succeed(`Schedule '${config.schedules[scheduleIndex].name}' enabled`);
          } else {
            // Stop the job if it's running
            if (activeJobs.has(id)) {
              activeJobs.get(id)?.stop();
              activeJobs.delete(id);
            }
            spinner.succeed(`Schedule '${config.schedules[scheduleIndex].name}' disabled`);
          }
        } catch (error) {
          spinner.fail('Failed to update schedule');
          console.error(chalk.red(`Error: ${error.message}`));
        }
      })
    );
  }
);

/**
 * CLI command that removes a schedule from disk and stops any active CRON job
 * for the same schedule id.
 */
const removeScheduleCommand = Command.make(
  'remove',
  {
    id: Args.text('ID').pipe(Args.withDescription('ID of the schedule to remove')),
  },
  ({ id }) => {
    return pipe(
      Effect.tryPromise(async () => {
        if (!id) {
          console.error(chalk.red('❌ Schedule ID is required'));
          return;
        }

        const spinner = ora('Removing schedule...').start();

        try {
          const config = await loadSchedules();
          const scheduleIndex = config.schedules.findIndex((s) => s.id === id);

          if (scheduleIndex === -1) {
            spinner.fail(`Schedule with ID '${id}' not found`);
            return;
          }

          const scheduleName = config.schedules[scheduleIndex].name;

          // Stop the job if it's running
          if (activeJobs.has(id)) {
            activeJobs.get(id)?.stop();
            activeJobs.delete(id);
          }

          // Remove the schedule
          config.schedules.splice(scheduleIndex, 1);

          // Save the updated config
          await saveSchedules(config);

          spinner.succeed(`Schedule '${scheduleName}' removed`);
        } catch (error) {
          spinner.fail('Failed to remove schedule');
          console.error(chalk.red(`Error: ${error.message}`));
        }
      })
    );
  }
);

/**
 * Top-level schedule-management command group.
 */
export const scheduleTxCommand = Command.make('schedule-tx')
  .pipe(Command.withDescription('Schedule transaction generation using CRON syntax'))
  .pipe(
    Command.withSubcommands([
      addScheduleCommand.pipe(Command.withDescription('Add a new transaction schedule')),
      listSchedulesCommand.pipe(Command.withDescription('List all transaction schedules')),
      toggleScheduleCommand.pipe(
        Command.withDescription('Enable or disable a transaction schedule')
      ),
      removeScheduleCommand.pipe(Command.withDescription('Remove a transaction schedule')),
    ])
  );

/**
 * Restore persisted schedules as soon as this command module is loaded.
 */
initSchedules();
