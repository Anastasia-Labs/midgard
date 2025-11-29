#!/usr/bin/env node
import { Command } from "commander";
import { Effect, Layer } from "effect";
import dotenv from "dotenv";
import { Lucid } from "./services/lucid.js";
import { AlwaysSucceedsContract } from "./services/always-succeeds.js";
import { NodeConfig } from "./services/config.js";
import { initializeMidgard } from "@/transactions/initialization.js";

dotenv.config();

const program = new Command();

program.name("midgard").description("Midgard L2 Protocol CLI").version("1.0.0");

program
  .command("init")
  .description("Initialize all Midgard contracts")
  .action(async () => {
    try {
      console.log("🚀 Initializing Midgard Protocol...\n");

      // Create layer and run Effect program
      const MainLayer = Layer.mergeAll(
        NodeConfig.layer,
        Lucid.Default,
        AlwaysSucceedsContract.Default,
      );

      const txHash = await Effect.runPromise(
        initializeMidgard.pipe(Effect.provide(MainLayer)),
      );

      console.log("\n✅ Midgard initialization complete!");
      console.log(`📝 Transaction: ${txHash}`);
      console.log(`🔗 https://preprod.cardanoscan.io/transaction/${txHash}`);

      process.exit(0);
    } catch (error) {
      console.error("\n❌ Initialization failed:", error);
      process.exit(1);
    }
  });

program.parse();
