/**
 * The single signed-deployment-authority fixture the watcher suites share.
 *
 * Every suite that admits a deployment needs the same thing before it can
 * assert anything: a deployment manifest whose contracts, reference scripts,
 * DA identity and release bindings all hang together, signed by a trust root
 * the watcher will accept. This module is the one copy, so a manifest-shape
 * change is made once and the suites cannot silently disagree about what a
 * valid deployment looks like.
 *
 * It calls `generateKeyPairSync` per call and leaves the result mutable,
 * which is what forgery and substitution tests depend on.
 */

import "node:crypto";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-core/deployment-profile";
import "@al-ft/midgard-core/out-ref";
import "@lucid-evolution/lucid";
import "../../src/runtime/deployment-identity.js";
import "../canonical-fraud-proof-catalogue.js";
import "./deployment-authority-fixture.build-watcher-authority-contracts.js";
import "./deployment-authority-fixture.build-watcher-deployment-authority-fixture.js";
export {
  addWatcherHistoryFixtureMetadata,
  asWireValue,
  type AuthorityContractFixture,
  type AuthorityReferenceScriptFixture,
  DA_SIGNERS_HASH,
  h28,
  h32,
  makeWatcherAuthorityContracts,
  NATIVE_SCRIPT_CBOR,
  NATIVE_SCRIPT_HASH,
  sha256,
  WATCHER_AUTHORITY_BLUEPRINT_HASH,
  WATCHER_AUTHORITY_PROGRAM_COMMITMENTS,
  WATCHER_AUTHORITY_RULE_BUNDLE_COMMITMENT,
  WATCHER_EMULATOR_HISTORY_RECIPE,
  WATCHER_HISTORY_FIXTURE_BOUNDS,
  WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
  type WatcherAuthorityContractSet,
  type WatcherDeploymentAuthorityFixtureOptions,
  type WatcherHistoryFixtureRecipe,
} from "./deployment-authority-fixture.build-watcher-authority-contracts.js";
export {
  makeDeploymentAuthority,
  makeWatcherDeploymentAuthorityFixture,
} from "./deployment-authority-fixture.build-watcher-deployment-authority-fixture.js";
