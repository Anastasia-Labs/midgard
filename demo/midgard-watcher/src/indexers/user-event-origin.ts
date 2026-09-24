/** Native activation facts. Event policy, coverage and store ownership live elsewhere. */
import {
  AddressData,
  CORRECTION_LOCK_ASSET_NAME,
  HubOracleDatum,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import {
  readWatcherLocalBackfillFinalityOriginalWitness,
  type WatcherLocalBackfillFinalityReceipt,
} from "../l1/finality-engine.js";
import type { WatcherLocalBackfillObservationReceipt } from "../l1/l1-adapter.js";
import {
  readWatcherUserEventScriptBinding,
  type VerifiedWatcherDeploymentIdentity,
  type WatcherUserEventScriptBinding,
} from "../runtime/deployment-identity.js";
import {
  watcherSameCanonicalJson,
  watcherSha256CanonicalJson,
} from "../storage/durable-store.js";

const ORIGIN_SCHEMA_VERSION = "midgard-watcher-user-event-origin-v1" as const;
const originBrand = Symbol("watcher-user-event-origin");
export type WatcherUserEventOriginReceipt = Readonly<{ [originBrand]: true }>;
type Scripts = ReturnType<typeof readWatcherUserEventScriptBinding>;
type OriginalWitness = ReturnType<
  typeof readWatcherLocalBackfillFinalityOriginalWitness
>;
type ActivationTransaction = Readonly<{
  transactionId: string;
  hubOutputIndex: number;
  hubOutRef: string;
  hubDatumCbor: string;
}>;
export type WatcherUserEventOriginFacts = Readonly<{
  schemaVersion: typeof ORIGIN_SCHEMA_VERSION;
  deploymentFingerprint: string;
  blueprintHash: string;
  blueprintSha256: string;
  network: Scripts["network"];
  canonicalOneShotOutRef: string;
  scripts: Scripts;
  block: OriginalWitness["current"]["observation"]["native"];
  parentPoint: OriginalWitness["current"]["observation"]["capture"]["predecessorPoint"];
  activation: ActivationTransaction & Readonly<{ transactionIndex: number }>;
  originalWitness: OriginalWitness;
  originDigest: string;
}>;
type OriginInput = Readonly<{
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  scriptBinding: WatcherUserEventScriptBinding;
  finality: WatcherLocalBackfillFinalityReceipt;
  observation: WatcherLocalBackfillObservationReceipt;
}>;
const origins = new WeakMap<
  WatcherUserEventOriginReceipt,
  Readonly<{
    input: OriginInput;
    facts: WatcherUserEventOriginFacts;
  }>
>();

const refuse = (reason: string): never => {
  throw new Error(`User-event origin rejected: ${reason}`);
};

const addressMatches = (address: AddressData, scriptHash: string): boolean =>
  Data.to(address, AddressData) ===
  Data.to(
    {
      paymentCredential: { ScriptCredential: [scriptHash] },
      stakeCredential: null,
    },
    AddressData,
  );

/** A semantic description only. Ledger validity and complete inclusion come from W12. */
const inspectActivationTransaction = (
  transactionCbor: string,
  scripts: Scripts,
): ActivationTransaction | null => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  if (transaction.to_cbor_hex() !== transactionCbor)
    refuse("transaction encoding differs");
  if (!transaction.is_valid()) return null;
  const body = transaction.body();
  const policy = CML.ScriptHash.from_hex(scripts.hub.policyId);
  const mint = body.mint();
  const hubAsset = CML.AssetName.from_hex(scripts.hub.assetName);
  if (mint?.get(policy, hubAsset) !== 1n) return null;
  const assets = mint.get_assets(policy);
  if (
    assets?.len() !== 2 ||
    mint.get(policy, CML.AssetName.from_hex(CORRECTION_LOCK_ASSET_NAME)) !== 1n
  ) {
    refuse("hub mint is not the exact positive hub/correction pair");
  }
  const inputs = body.inputs();
  let nonceCount = 0;
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    if (
      `${input.transaction_id().to_hex()}#${input.index().toString()}` ===
      scripts.canonicalOneShotOutRef
    )
      nonceCount += 1;
  }
  if (nonceCount !== 1)
    refuse(
      "activation does not consume the signed one-shot as an ordinary input",
    );
  const transactionId = CML.hash_transaction(body).to_hex();
  const outputs = body.outputs();
  let activation: ActivationTransaction | null = null;
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    const value = output.amount().multi_asset();
    if (value.get(policy, hubAsset) !== 1n) continue;
    if (activation !== null) refuse("activation has multiple hub outputs");
    if (
      value.keys().len() !== 1 ||
      value.get_assets(policy)?.len() !== 1 ||
      output.address().to_hex() !== scripts.hub.addressHex ||
      output.script_ref() !== undefined
    ) {
      refuse("hub output is not the authentic derived singleton");
    }
    const hubDatumCbor = output.datum()?.as_datum()?.to_cbor_hex();
    if (hubDatumCbor === undefined)
      return refuse("hub output has no inline datum");
    const datum = Data.from(hubDatumCbor, HubOracleDatum);
    if (
      CML.PlutusData.from_cbor_hex(
        Data.to(datum, HubOracleDatum),
      ).to_canonical_cbor_hex() !==
        CML.PlutusData.from_cbor_hex(hubDatumCbor).to_canonical_cbor_hex() ||
      datum.deposit !== scripts.deposit.policyId ||
      datum.withdrawal !== scripts.withdrawal.policyId ||
      datum.tx_order !== scripts.forcedOrder.policyId ||
      !addressMatches(datum.deposit_addr, scripts.deposit.spendScriptHash) ||
      !addressMatches(
        datum.withdrawal_addr,
        scripts.withdrawal.spendScriptHash,
      ) ||
      !addressMatches(datum.tx_order_addr, scripts.forcedOrder.spendScriptHash)
    ) {
      refuse("hub datum differs from the derived user-event scripts");
    }
    activation = Object.freeze({
      transactionId,
      hubOutputIndex: index,
      hubOutRef: `${transactionId}#${index.toString()}`,
      hubDatumCbor,
    });
  }
  return activation ?? refuse("activation has no authentic hub output");
};

const readInputs = (
  input: OriginInput,
): Readonly<{ scripts: Scripts; witness: OriginalWitness }> => {
  const scripts = readWatcherUserEventScriptBinding({
    binding: input.scriptBinding,
    deploymentIdentity: input.deploymentIdentity,
  });
  const witness = readWatcherLocalBackfillFinalityOriginalWitness({
    finality: input.finality,
    observation: input.observation,
  });
  for (const accepted of [witness.first, witness.current]) {
    const capture = accepted.observation.capture;
    if (
      capture.deploymentIdentityDigest !== scripts.deploymentFingerprint ||
      capture.blueprintHash !== scripts.blueprintHash ||
      capture.network !== scripts.network
    ) {
      refuse("capture differs from signed deployment identity");
    }
  }
  if (
    witness.first.observation.capture.nativeBlock.rawBlockCbor !==
      witness.current.observation.capture.nativeBlock.rawBlockCbor ||
    !watcherSameCanonicalJson(
      witness.first.observation.capture.predecessorPoint,
      witness.current.observation.capture.predecessorPoint,
    )
  ) {
    refuse("original and current activation blocks or parents differ");
  }
  return Object.freeze({ scripts, witness });
};

const sameRead = (
  left: ReturnType<typeof readInputs>,
  right: ReturnType<typeof readInputs>,
): boolean =>
  left.scripts === right.scripts &&
  left.witness.first === right.witness.first &&
  left.witness.current === right.witness.current;

/** Authenticates the activation block; indexing must process this whole block from its start. */
export const admitWatcherUserEventOrigin = (
  input: OriginInput,
): WatcherUserEventOriginReceipt => {
  const { deploymentIdentity, scriptBinding, finality, observation } = input;
  const owned = Object.freeze({
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
  });
  const accepted = readInputs(owned);
  const { scripts, witness } = accepted;
  const { native: block, capture } = witness.current.observation;
  const native = capture.nativeBlock;
  if (
    native.transactionCbors.length !== block.transactions.length ||
    native.transactionIds.length !== block.transactions.length
  ) {
    refuse("native transaction vector differs from the full observation");
  }
  let activation: WatcherUserEventOriginFacts["activation"] | null = null;
  for (
    let transactionIndex = 0;
    transactionIndex < native.transactionCbors.length;
    transactionIndex += 1
  ) {
    const candidate = inspectActivationTransaction(
      native.transactionCbors[transactionIndex]!,
      scripts,
    );
    if (candidate === null) continue;
    if (candidate.transactionId !== native.transactionIds[transactionIndex])
      refuse("activation transaction identity differs from native inclusion");
    if (activation !== null)
      refuse("block has multiple activation transactions");
    activation = Object.freeze({ ...candidate, transactionIndex });
  }
  if (activation === null)
    return refuse(
      "finalized native block has no activation for this deployment",
    );
  const identity = {
    schemaVersion: ORIGIN_SCHEMA_VERSION,
    deploymentFingerprint: scripts.deploymentFingerprint,
    blueprintHash: scripts.blueprintHash,
    blueprintSha256: scripts.blueprintSha256,
    network: scripts.network,
    canonicalOneShotOutRef: scripts.canonicalOneShotOutRef,
    scripts,
    point: capture.point,
    parentPoint: capture.predecessorPoint,
    blockContentDigest: block.blockContentDigest,
    nativeBlock: native,
    finalityBindingDigest: witness.current.finality.bindingDigest,
    firstAcquisitionDigest: witness.first.observation.acquisitionDigest,
    currentAcquisitionDigest: witness.current.observation.acquisitionDigest,
    activation,
  };
  const facts: WatcherUserEventOriginFacts = Object.freeze({
    schemaVersion: ORIGIN_SCHEMA_VERSION,
    deploymentFingerprint: scripts.deploymentFingerprint,
    blueprintHash: scripts.blueprintHash,
    blueprintSha256: scripts.blueprintSha256,
    network: scripts.network,
    canonicalOneShotOutRef: scripts.canonicalOneShotOutRef,
    scripts,
    block,
    parentPoint: capture.predecessorPoint,
    activation,
    originalWitness: witness,
    originDigest: watcherSha256CanonicalJson(identity),
  });
  if (!sameRead(accepted, readInputs(owned)))
    refuse("origin inputs changed during admission");
  const origin = Object.freeze({ [originBrand]: true as const });
  origins.set(origin, Object.freeze({ input: owned, facts }));
  return origin;
};

/** Rechecks the exact live capture pair; serialized facts cannot restore this receipt. */
export const readWatcherUserEventOrigin = (
  input: OriginInput &
    Readonly<{
      origin: WatcherUserEventOriginReceipt;
    }>,
): WatcherUserEventOriginFacts => {
  const { origin, deploymentIdentity, scriptBinding, finality, observation } =
    input;
  const owner = origins.get(origin);
  if (
    owner === undefined ||
    owner.input.deploymentIdentity !== deploymentIdentity ||
    owner.input.scriptBinding !== scriptBinding ||
    owner.input.finality !== finality ||
    owner.input.observation !== observation
  ) {
    return refuse(
      "origin or identity/observation pairing is not privately admitted",
    );
  }
  const current = readInputs(owner.input);
  if (
    !sameRead(current, {
      scripts: owner.facts.scripts,
      witness: owner.facts.originalWitness,
    })
  )
    refuse("origin accepted witness changed");
  return owner.facts;
};

/** Test-only semantic coverage from an existing signed frame; never issues origin authority. */
export const unsafeInspectWatcherUserEventActivationTransactionForTest = (
  input: Readonly<{
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    scriptBinding: WatcherUserEventScriptBinding;
    transactionCbor: string;
  }>,
): ActivationTransaction | null => {
  const { deploymentIdentity, scriptBinding, transactionCbor } = input;
  const scripts = readWatcherUserEventScriptBinding({
    binding: scriptBinding,
    deploymentIdentity,
  });
  return inspectActivationTransaction(transactionCbor, scripts);
};
