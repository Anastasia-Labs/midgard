#!/usr/bin/env node

import { Command } from "commander";
import { ENV_VARS_GUIDE, chalk } from "./utils.js";
import { runNode } from "./commands/index.js";
import packageJson from "../package.json" with { type: "json" };
import { Effect, pipe } from "effect";
import { NodeConfig, User } from "./config.js";
import dotenv from "dotenv";
import { AlwaysSucceeds } from "./services/index.js";
import { NodeRuntime } from "@effect/platform-node";

// Initialize global flags:
global.BLOCKS_IN_QUEUE = 0;
global.LATEST_SYNC_OF_STATE_QUEUE_LENGTH = 0;
global.RESET_IN_PROGRESS = false;

dotenv.config();
const VERSION = packageJson.version;

const program = new Command();

program.version(VERSION).description(
  `
  ${chalk.red(
    `                        @#
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

program.command("listen").action(async () => {
  const program = pipe(
    runNode,
    Effect.provide(User.layer),
    Effect.provide(AlwaysSucceeds.AlwaysSucceedsContract.layer),
    Effect.provide(NodeConfig.layer),
  );

  NodeRuntime.runMain(program);
});

program.parse(process.argv);
