/**
 * `da-bond`: operate the pooled DA committee bond (spec #685, ticket #691).
 *
 * - `status` reads the pool: state, backing against `da_bond_lovelace`, and
 *   `unlock_at` while a withdrawal is pending.
 * - `top-up` adds lovelace from any wallet (`TopUp` is permissionless).
 * - `withdraw begin|cancel|complete --build-unsigned <file>` builds an owner
 *   quorum transaction and writes it, unsigned, to a file; nothing is
 *   submitted. Each owner witnesses it offline with `da-bond witness`
 *   (`da-bond-files.ts`), and `assemble` checks the quorum and submits.
 *
 * The chain logic takes a `DaBondContext`, so emulator tests drive it without
 * a manifest; `loadDaBondContext` builds the production context from a
 * verified manifest and authenticates only what these commands read: the pool
 * reference scripts and the DA params governor.
 */

import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "@lucid-evolution/scalus-uplc";
import "effect";
import "@al-ft/midgard-core/ogmios-slot";
import "../lucid-time.js";
import "../services/native-ledger.js";
import "../transactions/utils.js";
import "./address-from-seed.js";
import "./availability-challenge-deployment.js";
import "./contract-deployment-info.js";
import "./da-bond-files.js";
import "./l1-utxos.js";
import "./da-bond.da-bond-context.js";
import "./da-bond.refusal-message.js";
import "./da-bond.da-bond-withdraw-build-command.js";
import "./da-bond.load-da-bond-context.js";
export {
  daBondAfterSubmitError,
  daBondChainSubmit,
  type DaBondContext,
  type DaBondStatus,
  daBondStatusCommand,
  daBondSubmitAndConfirm,
} from "./da-bond.da-bond-context.js";
export {
  daBondAssembleCommand,
  type DaBondChainOptions,
  DaBondCustomSlotMappingError,
  daBondWithdrawBuildCommand,
} from "./da-bond.da-bond-withdraw-build-command.js";
export {
  daBondLedgerTimeMs,
  daBondLucid,
  loadDaBondContext,
  runDaBondAssemble,
  runDaBondStatus,
  runDaBondTopUp,
  runDaBondWithdrawBuild,
} from "./da-bond.load-da-bond-context.js";
export {
  daBondTopUpCommand,
  type DaBondTopUpOptions,
  type DaBondWithdrawBuildOptions,
  type DaBondWithdrawStep,
} from "./da-bond.refusal-message.js";
