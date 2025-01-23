#!/usr/bin/env node

import { Command } from "commander";
import { chalk, logInfo, setupLucid } from "./utils.js";
import { initializeDb, listen } from "./commands/listen.js";
import * as packageJson from "../package.json";

const VERSION = packageJson.default.version;

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
           chalk.redBright("L  A  B  S")
       ) +
       "    "
   )}
  `
  )}
          ${"Midgard Node – Demo CLI Application"}`
);

program
  .command("listen")
  .option("-p, --port <number>", "Port to listen on", "3000")
  .option("--db-file-path <string>", "Path to SQLite DB file", "db")
  .option("-n, --network <string>", "Cardano network", "Preprod")
  .option("-i, --polling-interval <number>", "Time in milliseconds between each query of the chain to check whether the previous block had been registered", "10000")
  .option("--confirmed-state-polling-interval <number>", "Time in milliseconds between each query of the confirmed state to see if the next block can be merged", "600000")
  .option("--provider <string>", "Cardano provider", "Kupmios")
  .action(async (options) => {
    const lucid = await setupLucid(options.network, options.provider);
    const db = await initializeDb(options.dbFilePath)
    listen(lucid, db, options.port, options.pollingInterval, options.confirmedStatePollingInterval);
  });

program.parse(process.argv);
