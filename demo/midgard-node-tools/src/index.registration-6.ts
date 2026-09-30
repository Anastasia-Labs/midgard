import "./index.registration-5.js";

import { Effect } from "effect";
import {
  collectStringOption,
  failCli,
  parseStringListOption,
  provideDatabaseServices,
  runCliEffect,
  tapJson,
  writeJson,
} from "midgard-node/commands/cli-runtime";
import * as Services from "midgard-node/services/index";

import * as E2EFinalizeSummaryCommand from "./commands/e2e-finalize-summary.js";
import * as Phase4GenesisLedgerCommand from "./commands/phase4-genesis-ledger.js";
import * as StressCorpusCommand from "./commands/stress-corpus-generate.js";
import { l1KupmiosEnvironment } from "./environment.js";
import { parseTxEvidenceOptions, program } from "./index.registration.js";

program
  .command("stress-corpus-generate")
  .description(
    "Pre-build and verify an offline NDJSON corpus of signed Midgard L2 stress transactions",
  )
  .requiredOption("--target-rate-tps <rate>", "Target offered TPS")
  .requiredOption("--duration-ms <ms>", "Measured run duration in milliseconds")
  .option("--warmup-count <count>", "Warmup rows to reserve", "0")
  .option("--cooldown-count <count>", "Cooldown rows to reserve", "0")
  .option("--wallet-count <count>", "Override generated chain count")
  .option("--safety-factor <factor>", "Sizing safety factor", "1.1")
  .option("--amount-lovelace <amount>", "Self-transfer amount", "1000000")
  .option("--min-fee-a <amount>", "Midgard MIN_FEE_A; defaults to env")
  .option("--min-fee-b <amount>", "Midgard MIN_FEE_B; defaults to env")
  .option(
    "--max-submit-tx-cbor-bytes <bytes>",
    "Midgard MAX_SUBMIT_TX_CBOR_BYTES; defaults to env",
  )
  .option(
    "--assumed-acceptance-latency-ms <ms>",
    "Acceptance-latency bound used for wallet-count safety checks",
    "1000",
  )
  .option("--wallets-dir <path>", "Prepared stress wallet directory")
  .option("--out-dir <path>", "Output directory for corpus artifacts")
  .option("--workers <count>", "Worker thread count")
  .option("--slices <count>", "Number of corpus slice ids", "1")
  .option(
    "--slice-wallet-counts <counts>",
    "Comma-separated wallet counts for ordered, dependency-isolated slices (must sum to --wallet-count)",
  )
  .option("--corpus-slice-id-prefix <id>", "Corpus slice id prefix", "default")
  .option(
    "--rebuild-sample-rate <rate>",
    "Fraction of chains to rebuild and byte-compare during verification",
    "0.001",
  )
  .option(
    "--funding-source <source>",
    "Funding source mode: existing or fanout",
    "existing",
  )
  .option("--network <network>", "Mainnet or Preprod; defaults to NETWORK env")
  .option("--yes", "Confirm generation")
  .action(async (options) => {
    try {
      const config =
        StressCorpusCommand.parseStressCorpusGenerateConfig(options);
      const result = await StressCorpusCommand.generateStressCorpus(config);
      writeJson(result);
    } catch (error) {
      failCli("stress-corpus-generate", error);
    }
  });

program
  .command("stress-corpus-verify")
  .description("Stream-verify a generated Midgard stress corpus and sidecars")
  .requiredOption("--corpus-path <path>", "Corpus NDJSON path")
  .option("--index-path <path>", "Corpus index path")
  .option("--manifest-path <path>", "Corpus manifest path")
  .option(
    "--result-out <path>",
    "Write a SHA-bindable standalone verification result artifact",
  )
  .option(
    "--rebuild-wallets-dir <path>",
    "Prepared stress wallet directory for rebuild-sample verification",
  )
  .option(
    "--rebuild-sample-rate <rate>",
    "Fraction of chains to rebuild and byte-compare when --rebuild-wallets-dir is set",
    "0.001",
  )
  .option("--amount-lovelace <amount>", "Self-transfer amount", "1000000")
  .option("--min-fee-a <amount>", "Midgard MIN_FEE_A; defaults to env")
  .option("--min-fee-b <amount>", "Midgard MIN_FEE_B; defaults to env")
  .option(
    "--max-submit-tx-cbor-bytes <bytes>",
    "Midgard MAX_SUBMIT_TX_CBOR_BYTES; defaults to env",
  )
  .option("--network <network>", "Mainnet or Preprod; defaults to NETWORK env")
  .action(async (options) => {
    try {
      const config = StressCorpusCommand.parseStressCorpusVerifyConfig(options);
      const result = await StressCorpusCommand.verifyStressCorpus(config);
      writeJson(result);
    } catch (error) {
      failCli("stress-corpus-verify", error);
    }
  });

program
  .command("phase4-genesis-ledger")
  .description(
    "Explicitly seed or verify the complete configured L2 genesis set and A/B funding in an isolated Phase 4 local-devnet database",
  )
  .option("--seed", "Seed an empty run-scoped mempool ledger")
  .option(
    "--verify-only",
    "Require the complete byte-identical configured genesis ledger without mutating it",
  )
  .action((opts) => {
    const seed = opts.seed === true;
    const verifyOnly = opts.verifyOnly === true;
    if (seed === verifyOnly) {
      failCli(
        "phase4-genesis-ledger",
        new Error("Specify exactly one of --seed or --verify-only"),
      );
      return;
    }
    const mainEffect = Phase4GenesisLedgerCommand.phase4GenesisLedgerProgram({
      mode: seed ? "seed" : "verify",
    }).pipe(Effect.provide(Services.Database.layerWithNodeConfig), tapJson());
    runCliEffect(mainEffect);
  });

program
  .command("e2e-finalize-summary")
  .description(
    "Collect final e2e endpoint/database evidence and write summary.json plus summary.md",
  )
  .option("--out-dir <path>", "Output directory for summary artifacts")
  .option("--run-id <id>", "Stable run id for the summary")
  .option("--mode <mode>", "Run mode: attach, resume, fresh, or unknown")
  .option("--node-url <url>", "Midgard node URL")
  .option(
    "--admin-api-key-env <name>",
    "Environment variable that contains the admin API key",
    "ADMIN_API_KEY",
  )
  .option("--node-log <path>", "Raw Midgard node log artifact to link")
  .option(
    "--step-summary <path>",
    "Structured e2e-run-step summary JSON file to include; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--tx <label:txHash:status:source>",
    "Transaction evidence to include in the summary; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--stress-summary <path>",
    "Optional e2e-stress-l2-throughput summary.json artifact to include as a functional gate",
  )
  .option(
    "--state-correction-evidence <path>",
    "Launch-scope fault-proof aggregate claim; cannot satisfy acceptance without the independent source options",
  )
  .option(
    "--state-correction-deployment-manifest <path>",
    "Finalized Preprod deployment manifest independently loaded for state-correction acceptance",
  )
  .option(
    "--state-correction-blueprint <path>",
    "Aiken blueprint independently hashed for state-correction acceptance",
  )
  .option(
    "--state-correction-catalogue <path>",
    "Fraud-proof catalogue JSON independently matched to the deployment manifest",
  )
  .option(
    "--state-correction-parameters <path>",
    "Cardano protocol-parameter snapshot independently digested for state-correction acceptance",
  )
  .option(
    "--state-correction-workflow-journal <directory>",
    "Immutable completed family workflow journal directory; repeat once per launch-scope family",
    collectStringOption,
    [],
  )
  .option(
    "--state-correction-l1-observation <path>",
    "Authenticated local Kupmios/Ogmios L1 transaction observation; repeat for every required transaction",
    collectStringOption,
    [],
  )
  .option(
    "--state-correction-recovery-observation <path>",
    "Raw structured recovery drill observation; repeat in canonical recovery-matrix order",
    collectStringOption,
    [],
  )
  .option(
    "--state-correction-final-snapshot <path>",
    "Authenticated final chain/queue/economic/withdrawal/classification snapshot",
  )
  .action(async (_args, options) => {
    const opts = options.opts();
    const mode =
      opts.mode === "attach" ||
      opts.mode === "resume" ||
      opts.mode === "fresh" ||
      opts.mode === "unknown"
        ? opts.mode
        : "unknown";
    const adminApiKey =
      typeof opts.adminApiKeyEnv === "string"
        ? process.env[opts.adminApiKeyEnv]
        : undefined;
    const stateCorrectionWorkflowJournalDirectories = parseStringListOption(
      opts.stateCorrectionWorkflowJournal,
      "--state-correction-workflow-journal",
    );
    const stateCorrectionL1ObservationPaths = parseStringListOption(
      opts.stateCorrectionL1Observation,
      "--state-correction-l1-observation",
    );
    const stateCorrectionRecoveryObservationPaths = parseStringListOption(
      opts.stateCorrectionRecoveryObservation,
      "--state-correction-recovery-observation",
    );
    const stateCorrectionSingleSourceValues = [
      opts.stateCorrectionDeploymentManifest,
      opts.stateCorrectionBlueprint,
      opts.stateCorrectionCatalogue,
      opts.stateCorrectionParameters,
      opts.stateCorrectionFinalSnapshot,
    ];
    const hasAnyStateCorrectionIndependentSource =
      stateCorrectionSingleSourceValues.some(
        (value) => typeof value === "string",
      ) ||
      stateCorrectionWorkflowJournalDirectories.length > 0 ||
      stateCorrectionL1ObservationPaths.length > 0 ||
      stateCorrectionRecoveryObservationPaths.length > 0;
    const hasAllStateCorrectionIndependentSources =
      stateCorrectionSingleSourceValues.every(
        (value) => typeof value === "string",
      ) &&
      stateCorrectionWorkflowJournalDirectories.length > 0 &&
      stateCorrectionL1ObservationPaths.length > 0 &&
      stateCorrectionRecoveryObservationPaths.length > 0;
    if (
      hasAnyStateCorrectionIndependentSource &&
      !hasAllStateCorrectionIndependentSources
    ) {
      throw new Error(
        "State-correction independent reconciliation requires manifest, blueprint, catalogue, parameters, at least one workflow journal, at least one authenticated L1 observation, at least one recovery observation, and the final snapshot together.",
      );
    }
    const mainEffect = provideDatabaseServices(
      E2EFinalizeSummaryCommand.finalizeE2ESummaryProgram({
        ...(typeof opts.outDir === "string" ? { outDir: opts.outDir } : {}),
        ...(typeof opts.runId === "string" ? { runId: opts.runId } : {}),
        mode,
        ...(typeof opts.nodeUrl === "string" ? { nodeUrl: opts.nodeUrl } : {}),
        ...(adminApiKey === undefined ? {} : { adminApiKey }),
        ...(typeof opts.nodeLog === "string"
          ? { nodeLogPath: opts.nodeLog }
          : {}),
        stepSummaryPaths: parseStringListOption(
          opts.stepSummary,
          "--step-summary",
        ),
        transactions: parseTxEvidenceOptions(opts.tx),
        ...(typeof opts.stressSummary === "string"
          ? { stressSummaryPath: opts.stressSummary }
          : {}),
        ...(typeof opts.stateCorrectionEvidence === "string"
          ? {
              stateCorrectionEvidencePath: opts.stateCorrectionEvidence,
            }
          : {}),
        ...(hasAllStateCorrectionIndependentSources
          ? {
              stateCorrectionIndependentSourcePaths: {
                deploymentManifestPath:
                  opts.stateCorrectionDeploymentManifest as string,
                blueprintPath: opts.stateCorrectionBlueprint as string,
                cataloguePath: opts.stateCorrectionCatalogue as string,
                parametersPath: opts.stateCorrectionParameters as string,
                workflowJournalDirectories:
                  stateCorrectionWorkflowJournalDirectories,
                l1ObservationPaths: stateCorrectionL1ObservationPaths,
                recoveryObservationPaths:
                  stateCorrectionRecoveryObservationPaths,
                finalSnapshotPath: opts.stateCorrectionFinalSnapshot as string,
              },
              stateCorrectionLocalAuthorityConfig: l1KupmiosEnvironment(),
            }
          : {}),
      }).pipe(tapJson()),
    );

    runCliEffect(mainEffect);
  });
