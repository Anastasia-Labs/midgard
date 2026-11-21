/**
 * The DA libp2p runtime of the pooled DA bond journey's committee node
 * (ruling P31): the libp2p keys and the committee-target runtime manifest
 * that the observer of ruling P27 loads.
 *
 * The journey deployment writes neither, so the live adapter produces both
 * after the deploy and before the node's first spawn, through the same path
 * an operator uses after init: fresh libp2p keys, then the real
 * `midgard-node da-libp2p-generate-manifest --target committee` process. The
 * members' signer indexes and DA verification keys and the threshold come
 * from the finalized deployment manifest, never from hand-written JSON.
 *
 * The observer loads one member's libp2p identity, so the manifest's peer set
 * admits it. It never loads that member's DA signing key: libp2p identity is
 * transport-only, and `DA_SIGNER_INDEX` stays unset (P27(1)).
 *
 * Planning and the evidence checks are pure; the key writer, the process run
 * and the configuration check take their inputs explicitly, so both
 * polarities are testable without a devnet.
 */

import "node:child_process";
import "node:crypto";
import "node:fs";
import "node:path";
import "@al-ft/midgard-core/da-libp2p-identity";
import "da-committee-node/config";
import "da-committee-node/da/libp2p";
import "./da-bond-pool-committee-runtime.plan-da-bond-pool-committee-runtime.js";
import "./da-bond-pool-committee-runtime.reuse-da-bond-pool-committee-runtime.js";
export {
  DA_BOND_POOL_COMMITTEE_MEMBER_ROLES,
  DA_BOND_POOL_COMMITTEE_RUNTIME_MANIFEST,
  DA_BOND_POOL_LIBP2P_DEFAULT_PORTS,
  DA_BOND_POOL_LIBP2P_SECRETS,
  type DaBondPoolCommitteeDeployment,
  daBondPoolCommitteeRuntimeArgv,
  DaBondPoolCommitteeRuntimeError,
  type DaBondPoolCommitteeRuntimeMember,
  daBondPoolCommitteeRuntimeOptions,
  type DaBondPoolCommitteeRuntimePlan,
  planDaBondPoolCommitteeRuntime,
  readWorktreePortOffset,
} from "./da-bond-pool-committee-runtime.plan-da-bond-pool-committee-runtime.js";
export {
  type DaBondPoolCommitteeRuntimeEvidence,
  daBondPoolCommitteeSettings,
  type DaBondPoolRuntimeProcessResult,
  type DaBondPoolRuntimeProcessRunner,
  produceDaBondPoolCommitteeRuntime,
  reuseDaBondPoolCommitteeRuntime,
  spawnDaBondPoolRuntimeProcess,
  verifyDaBondPoolCommitteeRuntime,
  writeFreshDaLibp2pKey,
} from "./da-bond-pool-committee-runtime.reuse-da-bond-pool-committee-runtime.js";
