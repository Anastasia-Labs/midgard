#!/usr/bin/env node

import { Command } from "commander";
import { ENV_VARS_GUIDE, chalk } from "@/utils.js";
import { runNode } from "@/commands/index.js";
import * as Services from "@/services/index.js";
import packageJson from "../package.json" with { type: "json" };
import { Effect, pipe } from "effect";
import dotenv from "dotenv";
import { NodeRuntime } from "@effect/platform-node";
import { initializeMidgard } from "./transactions/initialization.js";

dotenv.config();
const VERSION = packageJson.version;

const program = new Command();

program.version(VERSION).description(
  `
  ${chalk.red(
    `                       @#
                         @@%#
                        %@@@%#
                       %%%%%%##
                      %%%%%%%%%#
                     %%%%%%%%%%%#
                    %%%%%%%%%%####
                   %%%%%%%%%#######
                  %%%%%%%%  ########
                 %%%%%%%%%  #########
                %%%%%%%%%%  ##########
               %%%%%%%%%%    ##########
              %%%%%%%%%%      ##########
             %%%%%%%%%%        ##########
            %%%%%%%%%%          ##########
           %%%%%%%%%%            ##########
          ###%%%%%%%              ##########
         #########                  #########

   ${chalk.bgGray(
     "    " +
       chalk.bold(
         chalk.whiteBright("A  N  A  S  T  A  S  I  A") +
           "     " +
           chalk.redBright("L  A  B  S"),
       ) +
       "    ",
   )}
  `,
  )}
          ${"Midgard Node – Demo CLI Application"}
  ${ENV_VARS_GUIDE}`,
);

program
  .command("listen")
  .option(
    "-m, --with-monitoring",
    "Flag for enabling interactions with monitoring services",
  )
  .action(async (_args, options) => {
    console.log("🌳 Midgard");
    const program = pipe(
      runNode(options.withMonitoring),
      Effect.provide(Services.Globals.Default),
      Effect.provide(Services.Database.layer),
      Effect.provide(Services.AlwaysSucceedsContract.Default),
      Effect.provide(Services.Lucid.Default),
      Effect.provide(Services.NodeConfig.layer),
      Effect.provide(Services.Globals.Default),
    );

    NodeRuntime.runMain(program, { teardown: undefined });
  });

  program
  .command("init")
  .description("Initialize Midgard contracts")
  .action(() => {
    const initProgram = pipe(
      initializeMidgard,
      Effect.provide(Services.AlwaysSucceedsContract.Default),
      Effect.provide(Services.Lucid.Default),
      Effect.provide(Services.NodeConfig.layer),
      Effect.tap(() =>
        Effect.logInfo("✅ Midgard contracts initialized successfully!"),
      ),
    );

    NodeRuntime.runMain(initProgram);
  });

program.parse(process.argv);
