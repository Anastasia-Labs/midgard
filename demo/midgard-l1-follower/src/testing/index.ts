/**
 * Test tooling of the L1 follower (plan §15 F8, §16.1): the fork
 * simulator, its scenario runner and corpus, and the fresh-replay
 * comparison. Never imported by production code.
 */
export {
  type FollowerProjection,
  mergeTrackedSets,
  projectionStoreOptions,
} from "../shadow/projection.js";
export {
  forkCorpus,
  forkEpisodeArbitrary,
  forkScenarioArbitrary,
  type NamedScenario,
} from "./arbitrary.js";
export {
  encodeBlock,
  type EncodedBlock,
  encodeTxBody,
  encodeUtxoAnswer,
  type SimBlock,
  type SimOutput,
  type SimTx,
  simTxHash,
} from "./block-cbor.js";
export * as cbor from "./cbor-writer.js";
export {
  EpisodeBuilder,
  FORK_SHAPES,
  type ForkCheck,
  type ForkCheckpoint,
  type ForkEpisode,
  type ForkScenario,
  type ForkShape,
  type ForkStep,
  type ScenarioTraffic,
} from "./episodes.js";
export {
  diffDumps,
  dumpStore,
  FACT_QUERIES,
  type StoreDump,
} from "./replay.js";
export { Rng } from "./rng.js";
export {
  buildForkSteps,
  checkpointFailure,
  type EventSource,
  type ForkRunOptions,
  type ForkRunOutcome,
  type ForkRunStats,
  runForkScenario,
  SIM_ORIGIN,
  simStoreOptions,
} from "./run-scenario.js";
export {
  CONWAY_BLOCK_TYPE,
  outRefHex,
  SimChain,
  type SimOrigin,
  type SimUniverse,
  simUniverse,
  type SimUtxo,
} from "./sim-chain.js";
export { type ForkWalletSeed, type SeedStats } from "./wallet-seed-run.js";
