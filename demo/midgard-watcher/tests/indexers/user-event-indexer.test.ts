import { mkdtemp, rm } from "node:fs/promises";
import { type Server } from "node:net";
import { join } from "node:path";

import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  deriveMidgardNativeTxProofSource,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec/native";
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec/native-constants";
import {
  DepositDatum,
  DepositSpendRedeemer,
  ForcedInclusionTx,
  HubOracleDatum,
  MerkleRoot,
  outputReferenceToPlutusDataCbor,
  PayoutDatum,
  PayoutMintRedeemer,
  Proof,
  resolveEventInclusionTime,
  RootDomain,
  SettlementDatum,
  TxOrderDatum,
  TxOrderMintRedeemer,
  TxOrderSpendRedeemer,
  UserEventMintRedeemer,
  UserEventWitnessPublishRedeemer,
  userEventWitnessScriptHash,
  WithdrawalOrderDatum,
  WithdrawalSpendRedeemer,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { blake2b } from "../../../midgard-core/node_modules/@noble/hashes/blake2.js";
import {
  deriveWatcherUserEventObservation as deriveWatcherUserEventObservationRaw,
  evaluateWatcherUserEventIndexer as evaluateWatcherUserEventIndexerRaw,
  makeWatcherUserEventIndexerPolicy,
  parseWatcherUserEventIndexerResult as parseWatcherUserEventIndexerResultRaw,
  parseWatcherUserEventIndexerState as parseWatcherUserEventIndexerStateRaw,
  WATCHER_USER_EVENT_INDEXER_BOUNDS,
  WATCHER_USER_EVENT_PUBLIC_CONTEXT_SCHEMA_VERSION,
  type WatcherUserEventIndexerPolicy,
  type WatcherUserEventIndexerState,
  type WatcherUserEventKind,
  type WatcherUserEventPublicContext,
} from "../../src/indexers/user-event-indexer.js";
import {
  evaluateWatcherFinality,
  makeWatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  closeWatcherL1TransportAttestationContext,
  encodeWatcherNormalizedL1Block,
  establishWatcherExternalProviderTransport,
  makeWatcherL1PublicBytes,
  normalizeWatcherL1Block as normalizeWatcherL1BlockRaw,
  WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
  type WatcherAuthenticatedL1Provider,
  type WatcherL1TransportAttestationContext,
  watcherL1TransportAttestationDetails,
} from "../../src/l1/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistency as evaluateWatcherMultiProviderConsistencyRaw } from "../../src/l1/multi-provider-consistency.js";
import {
  evaluateWatcherPostFinalityRecovery as evaluateWatcherPostFinalityRecoveryRaw,
  evaluateWatcherRollback as evaluateWatcherRollbackRaw,
  makeWatcherRollbackBootstrapState,
  type WatcherPostFinalityRecoveryInput,
} from "../../src/l1/rollback-engine.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import {
  encodeWatcherDurableStore,
  journalWatcherProtocolUtxoTransition,
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
  parseWatcherDurableStore,
  type WatcherDurableStore,
  watcherDurableStoreBytesSha256,
  type WatcherProtocolUtxo,
} from "../../src/storage/durable-store.js";
import { reorderWireKeys, sha256Canonical } from "../support/canonical-json.js";
import {
  asWireValue,
  h28,
  h32,
  makeDeploymentAuthority,
  sha256,
  WATCHER_AUTHORITY_RELEASE_DIGEST as RELEASE_DIGEST,
} from "../support/deployment-authority-fixture.js";
import { makeWatcherTlsTransportFixture } from "../support/tls-transport-fixture.js";

const encodeData = Data.to as unknown as (
  value: unknown,
  schema: unknown,
) => string;
/**
 * A user-event mint redeemer at the spelling the policy in question takes.
 *
 * The tx-order policy's redeemer is `user_events.MintRedeemer` inside its own
 * `MintRedeemer`, beside the §8 carriage vector (#594); the deposit, withdrawal
 * and witness policies take the bare enum. Every forced-order payload built here
 * is the canonically-empty transaction, so its vector is empty — §8.11's walk
 * consumes one entry per non-empty slot and there are none.
 */
const encodeUserEventMintRedeemerFor = (
  kind: WatcherUserEventKind,
  event: unknown,
  materialCarriage: readonly unknown[] = [],
): string =>
  kind === "forced_order"
    ? encodeData(
        { event, material_carriage: materialCarriage },
        TxOrderMintRedeemer,
      )
    : encodeData(event, UserEventMintRedeemer);

const adjacentConstructor = (cborHex: string): string => {
  const constructor =
    CML.PlutusData.from_cbor_hex(cborHex).as_constr_plutus_data()!;
  return CML.PlutusData.new_constr_plutus_data(
    CML.ConstrPlutusData.new(
      constructor.alternative() + 1n,
      constructor.fields(),
    ),
  ).to_cbor_hex();
};
const truncatedConstructor = (cborHex: string): string => {
  const constructor =
    CML.PlutusData.from_cbor_hex(cborHex).as_constr_plutus_data()!;
  const fields = constructor.fields();
  const truncated = CML.PlutusDataList.new();
  for (let index = 0; index + 1 < fields.len(); index += 1) {
    truncated.add(fields.get(index));
  }
  return CML.PlutusData.new_constr_plutus_data(
    CML.ConstrPlutusData.new(constructor.alternative(), truncated),
  ).to_cbor_hex();
};
const truncatedList = (cborHex: string): string => {
  const fields = CML.PlutusData.from_cbor_hex(cborHex).as_list()!;
  const truncated = CML.PlutusDataList.new();
  for (let index = 0; index + 1 < fields.len(); index += 1) {
    truncated.add(fields.get(index));
  }
  return CML.PlutusData.new_list(truncated).to_cbor_hex();
};
const scriptAddress = (scriptHash: string): string => `70${scriptHash}`;

type MutableRecord = Record<string, any>;
const transportEndpointByProviderId = new Map<string, string>();

const deploymentAuthorityFixture = makeDeploymentAuthority();
const applied = deploymentAuthorityFixture.policy.appliedScriptHashes;

const eventFields = {
  deposit: {
    policyId: applied.depositMint!,
    spendScriptHash: applied.depositSpend!,
    addressHex: scriptAddress(applied.depositSpend!),
  },
  withdrawal: {
    policyId: applied.withdrawalMint!,
    spendScriptHash: applied.withdrawalSpend!,
    addressHex: scriptAddress(applied.withdrawalSpend!),
  },
  forcedOrder: {
    policyId: applied.txOrderMint!,
    spendScriptHash: applied.txOrderSpend!,
    addressHex: scriptAddress(applied.txOrderSpend!),
  },
} as const;
const asAddressData = (addressHex: string) => ({
  paymentCredential: {
    ScriptCredential: [addressHex.slice(2)],
  } as { ScriptCredential: [string] },
  stakeCredential: null,
});
const hubDatum = {
  registered_operators: applied.registeredOperatorsMint!,
  active_operators: applied.activeOperatorsMint!,
  retired_operators: applied.retiredOperatorsMint!,
  scheduler: applied.schedulerMint!,
  state_queue: applied.stateQueueMint!,
  fraud_proof_catalogue: applied.fraudProofCatalogueMint!,
  fraud_proof: applied.fraudProofMint!,
  deposit: eventFields.deposit.policyId,
  withdrawal: eventFields.withdrawal.policyId,
  tx_order: eventFields.forcedOrder.policyId,
  settlement: applied.settlementMint!,
  payout: applied.payoutMint!,
  registered_operators_addr: asAddressData(
    scriptAddress(applied.registeredOperatorsSpend!),
  ),
  active_operators_addr: asAddressData(
    scriptAddress(applied.activeOperatorsSpend!),
  ),
  retired_operators_addr: asAddressData(
    scriptAddress(applied.retiredOperatorsSpend!),
  ),
  scheduler_addr: asAddressData(scriptAddress(applied.schedulerSpend!)),
  state_queue_addr: asAddressData(scriptAddress(applied.stateQueueSpend!)),
  fraud_proof_catalogue_addr: asAddressData(
    scriptAddress(applied.fraudProofCatalogueSpend!),
  ),
  fraud_proof_addr: asAddressData(scriptAddress(applied.fraudProofSpend!)),
  deposit_addr: asAddressData(eventFields.deposit.addressHex),
  withdrawal_addr: asAddressData(eventFields.withdrawal.addressHex),
  tx_order_addr: asAddressData(eventFields.forcedOrder.addressHex),
  settlement_addr: asAddressData(scriptAddress(applied.settlementSpend!)),
  reserve_addr: asAddressData(scriptAddress(applied.reserveSpend!)),
  payout_addr: asAddressData(scriptAddress(applied.payoutSpend!)),
  reserve_observer: applied.reserveWithdraw!,
};
const hubDatumHex = Data.to(hubDatum, HubOracleDatum);
const hubAssets = CML.MultiAsset.new();
hubAssets.set(
  CML.ScriptHash.from_hex(applied.hubOracleMint!),
  CML.AssetName.from_hex(""),
  1n,
);
const hubOutput = CML.TransactionOutput.new(
  CML.Address.from_hex(scriptAddress(applied.hubOracleMint!)),
  CML.Value.new(5_000_000n, hubAssets),
  CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(hubDatumHex)),
);
const HUB_OUT_REF = `${h32("a0")}#0`;
const SETTLEMENT_OUT_REF = `${h32("a3")}#0`;
const MEMBERSHIP_PHAS_ROOT = h32("a4");
const countedRoot = (
  domain:
    | "DepositsRootDomain"
    | "WithdrawalsRootDomain"
    | "ForcedTransactionsV1RootDomain",
): string =>
  Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from("MidgardRootCountV1", "utf8"),
        Buffer.from(Data.to(domain as never, RootDomain as never), "hex"),
        Buffer.from(MEMBERSHIP_PHAS_ROOT, "hex"),
        Buffer.from(Data.to(1n as never, Data.Integer() as never), "hex"),
      ]),
      { dkLen: 32 },
    ),
  ).toString("hex");
const settlementDatum = {
  deposits_root: countedRoot("DepositsRootDomain"),
  withdrawals_root: countedRoot("WithdrawalsRootDomain"),
  forced_transactions_root: countedRoot("ForcedTransactionsV1RootDomain"),
  transactions_root: h32("a5"),
  resolution_claim: null,
};
const settlementAssets = CML.MultiAsset.new();
settlementAssets.set(
  CML.ScriptHash.from_hex(applied.settlementMint!),
  CML.AssetName.from_hex(""),
  1n,
);
const settlementOutput = CML.TransactionOutput.new(
  CML.Address.from_hex(scriptAddress(applied.settlementSpend!)),
  CML.Value.new(5_000_000n, settlementAssets),
  CML.DatumOption.new_datum(
    CML.PlutusData.from_cbor_hex(Data.to(settlementDatum, SettlementDatum)),
  ),
);
const BOOTSTRAP_CHAIN_POINT_ID = h32("a1");
const bootstrapStore = makeWatcherDurableStore({
  deploymentMarker: deploymentAuthorityFixture.marker,
  revision: "0",
  records: {
    l1Observations: [],
    chainPoints: [
      {
        chainPointId: BOOTSTRAP_CHAIN_POINT_ID,
        providerId: "provider-a",
        blockHash: h32("a2"),
        slot: "1",
        blockNo: "1",
        depth: "10",
      },
    ],
    protocolUtxos: [
      {
        outRef: HUB_OUT_REF,
        role: "hub_oracle",
        chainPointId: BOOTSTRAP_CHAIN_POINT_ID,
        output: makeWatcherDurablePayload(hubOutput.to_cbor_hex()),
      },
      {
        outRef: SETTLEMENT_OUT_REF,
        role: "settlement",
        chainPointId: BOOTSTRAP_CHAIN_POINT_ID,
        output: makeWatcherDurablePayload(settlementOutput.to_cbor_hex()),
      },
    ],
    daProofInputs: [],
    reconstructedStates: [],
    decisions: [],
    faults: [],
    submissions: [],
    confirmations: [],
    retries: [],
    deadlines: [],
    correctionResults: [],
  },
});
const deploymentAuthority = {
  signedIdentity: deploymentAuthorityFixture.signedIdentity,
  policy: deploymentAuthorityFixture.policy,
  trustRoots: deploymentAuthorityFixture.trustRoots,
  result: deploymentAuthorityFixture.result,
};

const emptyNativeTxCanonical: MidgardNativeTxCanonical = {
  version: MIDGARD_NATIVE_TX_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: EMPTY_CBOR_LIST,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: Buffer.alloc(32),
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  },
};
const emptyNativeTx = materializeMidgardNativeTxFromCanonical(
  emptyNativeTxCanonical,
);
const emptyNativeSource = deriveMidgardNativeTxProofSource(emptyNativeTx);
const emptyNativePayload = {
  tx_id: computeMidgardNativeTxId(emptyNativeTx).toString("hex"),
  transaction_commitment:
    computeMidgardNativeTxProofCommitment(emptyNativeSource).toString("hex"),
  source: {
    compact_cbor: emptyNativeSource.compactCbor.toString("hex"),
    witness_set_compact_cbor:
      emptyNativeSource.witnessSetCompactCbor.toString("hex"),
    field_preimage_lengths_cbor:
      emptyNativeSource.fieldPreimageLengthsCbor.toString("hex"),
  },
};
const nonEmptyNativeCanonical: MidgardNativeTxCanonical = {
  ...emptyNativeTxCanonical,
  body: {
    ...emptyNativeTxCanonical.body,
    requiredSignersPreimageCbor: encodeCbor([Buffer.alloc(28, 0x77)]),
  },
};
const nonEmptyNativeTx = materializeMidgardNativeTxFromCanonical(
  nonEmptyNativeCanonical,
);
const nonEmptyNativeSource = deriveMidgardNativeTxProofSource(nonEmptyNativeTx);
const nonEmptyNativePayload = {
  tx_id: computeMidgardNativeTxId(nonEmptyNativeTx).toString("hex"),
  transaction_commitment:
    computeMidgardNativeTxProofCommitment(nonEmptyNativeSource).toString("hex"),
  source: {
    compact_cbor: nonEmptyNativeSource.compactCbor.toString("hex"),
    witness_set_compact_cbor:
      nonEmptyNativeSource.witnessSetCompactCbor.toString("hex"),
    field_preimage_lengths_cbor:
      nonEmptyNativeSource.fieldPreimageLengthsCbor.toString("hex"),
  },
};

/**
 * The §8 carriage vector a forced order over `nonEmptyNativeCanonical` supplies.
 *
 * Exactly one of §2.5's nine slots is non-empty there — field 4, required signers
 * — so the vector is one entry long and carries that field's own §5.1 preimage
 * inline. §8.11's vector is positional over the non-empty slots and MUST be
 * exhausted exactly, so this is not a free choice: one entry is what the tx-order
 * mint accepts for this payload, and the indexer re-derives that count.
 */
const nonEmptyNativeCarriage = Object.freeze([
  Object.freeze({
    Inline: Object.freeze({
      preimage: Buffer.from(
        nonEmptyNativeCanonical.body.requiredSignersPreimageCbor,
      ).toString("hex"),
    }),
  }),
]);

const policy = makeWatcherUserEventIndexerPolicy({
  network: "Preprod",
  releaseEvidenceDigest: RELEASE_DIGEST,
  deploymentMarker: deploymentAuthorityFixture.marker,
  deposit: eventFields.deposit,
  withdrawal: eventFields.withdrawal,
  forcedOrder: eventFields.forcedOrder,
  bootstrapStoreDigest: watcherDurableStoreBytesSha256(
    encodeWatcherDurableStore(bootstrapStore),
  ),
  deploymentTrustRootId: deploymentAuthorityFixture.result.trustRootId,
  requiredFinalityDepth: "2",
  maximumActiveHistoryEntries: "32",
  maximumAuditHistoryEntries: "128",
}) as WatcherUserEventIndexerPolicy;

const makeExternalFinalityPolicy = () =>
  makeWatcherFinalityPolicy(
    {
      schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
      mode: "development",
      targetNetwork: "Preprod",
      l1: {
        source: {
          sourceMode: "external_providers",
          providers: [
            {
              identity: "provider-a",
              operatorIdentitySha256: h32("97"),
              endpoint:
                transportEndpointByProviderId.get("provider-a") ??
                "https://cardano-a.example",
            },
            {
              identity: "provider-b",
              operatorIdentitySha256: h32("98"),
              endpoint:
                transportEndpointByProviderId.get("provider-b") ??
                "https://cardano-b.example",
            },
          ],
        },
        requestTimeoutMs: 10_000,
        maxConcurrency: 4,
        finality: {
          depth: 2,
          rollback: {
            beforeFinality: "rewind",
            afterFinality: "quarantine",
            maxDepth: 2,
          },
        },
      },
      da: {
        peers: [
          {
            identity: "da-peer-a",
            multiaddr:
              "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
          },
        ],
        requestTimeoutMs: 10_000,
        maxConcurrency: 4,
      },
      storage: {
        driver: "sqlite",
        path: "/var/lib/midgard-watcher/watcher.sqlite",
        rollbackAuthorityKeySource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
        },
      },
      proverWallet: {
        keySource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_PROVER_KEY",
        },
      },
      deadlines: {
        daFetchMs: 60_000,
        daPublishMs: 60_000,
        proofConstructMs: 300_000,
        proofSubmitMs: 120_000,
      },
    },
    {
      manifestId: policy.deploymentMarker.manifestId,
      network: "Preprod",
      trustRootId: h32("33"),
      releaseEvidenceDigest: policy.releaseEvidenceDigest,
      ruleBundleCommitment: h32("44"),
      programCommitments: { validation: h32("55") },
      durableMarker: policy.deploymentMarker,
    },
  )!;
let finalityPolicy: NonNullable<ReturnType<typeof makeWatcherFinalityPolicy>>;

let provider: WatcherAuthenticatedL1Provider;
let providerB: WatcherAuthenticatedL1Provider;
const externalSource = {
  sourceMode: "external_providers",
  network: "Preprod",
  providers: [
    {
      providerId: "provider-a",
      operatorIdentitySha256: h32("97"),
      endpoint: "https://cardano-a.example",
    },
    {
      providerId: "provider-b",
      operatorIdentitySha256: h32("98"),
      endpoint: "https://cardano-b.example",
    },
  ],
} as const;
const watcherTransportContexts: WatcherL1TransportAttestationContext[] = [];
const normalizedTransportContexts = new WeakMap<
  object,
  WatcherL1TransportAttestationContext
>();
const watcherTransportServers: Server[] = [];
let watcherTransportFixtureDirectory = "";

const makeTlsTransportFixture = async (name: string) =>
  await makeWatcherTlsTransportFixture(
    watcherTransportFixtureDirectory,
    watcherTransportServers,
    name,
  );

beforeAll(async () => {
  watcherTransportFixtureDirectory = await mkdtemp(
    join("/dev/shm", "midgard-w17-transports-"),
  );
  const externalTransports = await Promise.all(
    [
      ["provider-a", h32("97")],
      ["provider-b", h32("98")],
    ].map(async ([providerId, operatorIdentitySha256]) => {
      const fixture = await makeTlsTransportFixture(providerId!);
      const endpoint = `https://localhost:${fixture.port}`;
      transportEndpointByProviderId.set(providerId!, endpoint);
      const configuredProvider = externalSource.providers.find(
        ({ providerId: configuredProviderId }) =>
          configuredProviderId === providerId,
      );
      if (configuredProvider === undefined) {
        throw new Error("missing external-provider fixture policy");
      }
      (configuredProvider as MutableRecord).endpoint = endpoint;
      return await establishWatcherExternalProviderTransport({
        network: "Preprod",
        providerId: providerId!,
        operatorIdentitySha256: operatorIdentitySha256!,
        endpoint,
        caPem: fixture.certificate,
        expectedTlsPublicIdentitySha256: fixture.identitySha256,
        connectTimeoutMs: 2_000,
      });
    }),
  );
  watcherTransportContexts.push(...externalTransports);
  finalityPolicy = makeExternalFinalityPolicy();
  provider = watcherL1TransportAttestationDetails(
    externalTransports[0],
  )!.provider;
  providerB = watcherL1TransportAttestationDetails(
    externalTransports[1],
  )!.provider;
});

afterAll(async () => {
  for (const context of watcherTransportContexts) {
    closeWatcherL1TransportAttestationContext(context);
  }
  for (const server of watcherTransportServers) {
    server.close();
  }
  await rm(watcherTransportFixtureDirectory, {
    recursive: true,
    force: true,
  });
});

const transportForProvider = (
  authenticatedProvider: unknown,
): WatcherL1TransportAttestationContext => {
  const matches = watcherTransportContexts.filter((context) => {
    const details = watcherL1TransportAttestationDetails(context);
    return (
      details !== null &&
      JSON.stringify(details.provider) === JSON.stringify(authenticatedProvider)
    );
  });
  if (matches.length !== 1) {
    throw new Error("test provider lacks one live transport attestation");
  }
  return matches[0]!;
};

const normalizeWatcherL1Block = (
  authenticatedProvider: unknown,
  observation: unknown,
) => {
  const transport = transportForProvider(authenticatedProvider);
  const normalized = normalizeWatcherL1BlockRaw(transport, observation);
  normalizedTransportContexts.set(normalized, transport);
  return normalized;
};

const evaluateWatcherMultiProviderConsistency = (
  configuredSource: unknown,
  observations: readonly unknown[],
) =>
  evaluateWatcherMultiProviderConsistencyRaw(
    configuredSource,
    observations,
    observations.map((observation) => {
      const transport =
        typeof observation === "object" && observation !== null
          ? normalizedTransportContexts.get(observation)
          : undefined;
      if (transport === undefined) {
        throw new Error("test observation lacks live transport provenance");
      }
      return transport;
    }),
  );

const deriveWatcherUserEventObservation = (
  policyInput: unknown,
  previousStateInput: unknown,
  publicContextInput: unknown,
  rollbackTargetEntryDigest: string | null = null,
) =>
  deriveWatcherUserEventObservationRaw(
    policyInput,
    previousStateInput,
    publicContextInput,
    watcherTransportContexts,
    rollbackTargetEntryDigest,
  );

const evaluateWatcherUserEventIndexer = (
  policyInput: unknown,
  previousStateInput: unknown,
  observationInput: unknown,
  publicContextInput: unknown,
) =>
  evaluateWatcherUserEventIndexerRaw(
    policyInput,
    previousStateInput,
    observationInput,
    publicContextInput,
    watcherTransportContexts,
  );

const parseWatcherUserEventIndexerState = (
  value: unknown,
  policyInput: unknown,
) =>
  parseWatcherUserEventIndexerStateRaw(
    value,
    policyInput,
    watcherTransportContexts,
  );

const parseWatcherUserEventIndexerResult = (
  value: unknown,
  context: Omit<
    Parameters<typeof parseWatcherUserEventIndexerResultRaw>[1],
    "transportAttestations"
  >,
) =>
  parseWatcherUserEventIndexerResultRaw(value, {
    ...context,
    transportAttestations: watcherTransportContexts,
  });

const evaluateWatcherRollback = (
  policyInput: unknown,
  storeInput: unknown,
  previousFinalityStateInput: unknown,
  consistencyInput: unknown,
  finalityResultInput: unknown,
  previousRollbackStateInput: unknown,
  rollbackBootstrapStateInput: unknown,
  trustedCheckpointAuthorityInput: unknown = undefined,
) =>
  evaluateWatcherRollbackRaw(
    policyInput,
    storeInput,
    previousFinalityStateInput,
    consistencyInput,
    finalityResultInput,
    previousRollbackStateInput,
    rollbackBootstrapStateInput,
    trustedCheckpointAuthorityInput,
    watcherTransportContexts,
  );

const evaluateWatcherPostFinalityRecovery = (
  input: WatcherPostFinalityRecoveryInput,
) =>
  evaluateWatcherPostFinalityRecoveryRaw({
    ...input,
    transportAttestations: watcherTransportContexts,
  });

const addressData = {
  paymentCredential: {
    PublicKeyCredential: [h28("88")],
  },
  stakeCredential: null,
} as const;

const nonceAssetName = (txHash: string, outputIndex: number): string => {
  const eventId = outputReferenceToPlutusDataCbor({ txHash, outputIndex });
  return Buffer.from(
    blake2b(Buffer.from(eventId, "hex"), { dkLen: 32 }),
  ).toString("hex");
};

type EventFixture = Readonly<{
  kind: WatcherUserEventKind;
  output: CML.TransactionOutput;
  outputCborHex: string;
  datumCborHex: string;
  assetNameHex: string;
  nonceInput: CML.TransactionInput;
  mintRedeemerHex: string;
  certificateRedeemerHex: string;
  certificate: CML.Certificate;
  fields: WatcherUserEventIndexerPolicy[
    | "deposit"
    | "withdrawal"
    | "forcedOrder"];
  extraReferenceOutRefs: readonly string[];
  /** The tx-order policy's §8 carriage vector; empty at the other policies. */
  materialCarriage: readonly unknown[];
}>;

const makeEventFixture = (
  kind: WatcherUserEventKind,
  nonceByte: string,
  nonceIndex: number,
  ttl: bigint,
  inclusionTimeDelta = 0n,
  addressOverride?: string,
  witnessOverride?: string,
  forcedPayloadOverride?: Readonly<{
    tx_id: string;
    transaction_commitment: string;
    source: Readonly<{
      compact_cbor: string;
      witness_set_compact_cbor: string;
      field_preimage_lengths_cbor: string;
    }>;
  }>,
  extraReferenceOutRefs: readonly string[] = [],
  materialCarriage: readonly unknown[] = [],
): EventFixture => {
  const fields =
    kind === "deposit"
      ? policy.deposit
      : kind === "withdrawal"
        ? policy.withdrawal
        : policy.forcedOrder;
  const nonceHash = h32(nonceByte);
  const nonceInput = CML.TransactionInput.new(
    CML.TransactionHash.from_hex(nonceHash),
    BigInt(nonceIndex),
  );
  const assetNameHex = nonceAssetName(nonceHash, nonceIndex);
  const witness = witnessOverride ?? userEventWitnessScriptHash(assetNameHex);
  const eventId = {
    transactionId: nonceHash,
    outputIndex: BigInt(nonceIndex),
  };
  const inclusion_time =
    BigInt(resolveEventInclusionTime(Number(ttl), "Preprod")) +
    inclusionTimeDelta;
  const datum =
    kind === "deposit"
      ? {
          event: {
            id: eventId,
            info: {
              l2_address: addressData,
              l2_network_id: 0n,
              l2_datum: null,
            },
          },
          inclusion_time,
          witness,
        }
      : kind === "withdrawal"
        ? {
            event: {
              id: eventId,
              info: {
                body: {
                  l2_outref: eventId,
                  l2_owner: h28("89"),
                  l2_value: new Map<string, Map<string, bigint>>(),
                  l1_address: addressData,
                  l1_datum: "NoDatum" as const,
                },
                signature: ["aa", "bb"] as [string, string],
                validity: "WithdrawalIsValid" as const,
              },
            },
            inclusion_time,
            witness,
            refund_address: addressData,
            refund_datum: "NoDatum" as const,
          }
        : {
            event: {
              id: eventId,
              tx: forcedPayloadOverride ?? emptyNativePayload,
            },
            inclusion_time,
            witness,
            refund_address: addressData,
            refund_datum: "NoDatum" as const,
          };
  const schema =
    kind === "deposit"
      ? DepositDatum
      : kind === "withdrawal"
        ? WithdrawalOrderDatum
        : TxOrderDatum;
  const datumCborHex = CML.PlutusData.from_cbor_hex(
    Data.to(datum as never, schema as never),
  ).to_canonical_cbor_hex();
  const multiasset = CML.MultiAsset.new();
  multiasset.set(
    CML.ScriptHash.from_hex(fields.policyId),
    CML.AssetName.from_hex(assetNameHex),
    1n,
  );
  const output = CML.TransactionOutput.new(
    CML.Address.from_hex(addressOverride ?? fields.addressHex),
    CML.Value.new(3_000_000n, multiasset),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumCborHex)),
  );
  const certificate = CML.Certificate.new_reg_cert(
    CML.Credential.new_script(CML.ScriptHash.from_hex(witness)),
    0n,
  );
  return {
    kind,
    output,
    outputCborHex: output.to_canonical_cbor_hex(),
    datumCborHex,
    assetNameHex,
    nonceInput,
    materialCarriage,
    mintRedeemerHex: encodeUserEventMintRedeemerFor(
      kind,
      {
        AuthenticateEvent: {
          nonce_input_index: 0n,
          event_output_index: 0n,
          hub_ref_input_index: 0n,
          witness_registration_redeemer_index: 0n,
        },
      },
      materialCarriage,
    ),
    certificateRedeemerHex: encodeData(
      { MintOrBurn: { targetPolicy: fields.policyId } },
      UserEventWitnessPublishRedeemer,
    ),
    certificate,
    fields,
    extraReferenceOutRefs,
  };
};

type BlockBundle = Readonly<{
  context: WatcherUserEventPublicContext;
  store: ReturnType<typeof makeWatcherDurableStore>;
  transactionHash: string | null;
  finalityState: unknown;
}>;

type UserEventFinalityLineage = NonNullable<
  WatcherUserEventPublicContext["finalityAuthority"]
>["lineage"];

const userEventFinalityLineageByStateDigest = new Map<
  string,
  UserEventFinalityLineage
>();

const USER_EVENT_SCRIPT_DATA_HASH = CML.ScriptDataHash.from_raw_bytes(
  Buffer.alloc(32, 0x6a),
);

const userEventRedeemerTag = (purpose: string): CML.RedeemerTag => {
  switch (purpose) {
    case "spend":
      return CML.RedeemerTag.Spend;
    case "mint":
      return CML.RedeemerTag.Mint;
    case "certificate":
      return CML.RedeemerTag.Cert;
    case "withdrawal":
      return CML.RedeemerTag.Reward;
    default:
      throw new Error(`unsupported test redeemer purpose: ${purpose}`);
  }
};

const contextFromTransaction = (
  transaction: {
    readonly txHash: string;
    readonly body: ReturnType<typeof makeWatcherL1PublicBytes>;
    readonly utxos: readonly unknown[];
    readonly scripts: readonly never[];
    readonly datums: readonly never[];
    readonly redeemers: readonly unknown[];
  } | null,
  priorStore: ReturnType<typeof makeWatcherDurableStore> | null,
  nextProtocolUtxos: readonly WatcherProtocolUtxo[],
  blockNo: number,
  depth: number,
  previousFinalityState: unknown = null,
  pointOverride?: Readonly<{
    blockHash: string;
    parentBlockHash: string | null;
    slot: string;
    blockNo: string;
    depth: string;
  }>,
  transactionIsValid = true,
): BlockBundle => {
  serial += 1;
  const sourceStore = priorStore ?? bootstrapStore;
  const authenticatedProvider = provider;
  const selectedFinalityPolicy = finalityPolicy;
  const currentBlockNo = BigInt(pointOverride?.blockNo ?? blockNo.toString());
  const parentHash =
    [...sourceStore.chainPoints]
      .filter(
        ({ blockNo: priorBlockNo }) => BigInt(priorBlockNo) < currentBlockNo,
      )
      .sort((left, right) =>
        BigInt(left.blockNo) < BigInt(right.blockNo) ? 1 : -1,
      )
      .at(0)?.blockHash ?? null;
  const canonicalTransaction =
    transaction === null
      ? null
      : (() => {
          const body = CML.TransactionBody.from_cbor_hex(
            transaction.body.bytesHex,
          );
          const canonicalRedeemers = transaction.redeemers.map((candidate) => {
            const redeemer = candidate as MutableRecord;
            const bytes = redeemer.bytes as ReturnType<
              typeof makeWatcherL1PublicBytes
            >;
            return {
              ...redeemer,
              bytes: makeWatcherL1PublicBytes(
                CML.PlutusData.from_cbor_hex(
                  bytes.bytesHex,
                ).to_canonical_cbor_hex(),
              ),
            };
          });
          const witnessSet = CML.TransactionWitnessSet.new();
          if (canonicalRedeemers.length > 0) {
            const redeemers = CML.LegacyRedeemerList.new();
            for (const candidate of canonicalRedeemers) {
              const redeemer = candidate as MutableRecord;
              const bytes = redeemer.bytes as ReturnType<
                typeof makeWatcherL1PublicBytes
              >;
              redeemers.add(
                CML.LegacyRedeemer.new(
                  userEventRedeemerTag(String(redeemer.purpose)),
                  BigInt(String(redeemer.index)),
                  CML.PlutusData.from_cbor_hex(bytes.bytesHex),
                  CML.ExUnits.new(0n, 0n),
                ),
              );
            }
            witnessSet.set_redeemers(
              CML.Redeemers.new_arr_legacy_redeemer(redeemers),
            );
          }
          const fullTransaction = CML.Transaction.new(
            body,
            witnessSet,
            transactionIsValid,
            undefined,
          );
          const appliedUtxos = Array.from(
            { length: body.outputs().len() },
            (_, index) => {
              const output = body.outputs().get(index);
              const datum = output.datum()?.as_datum();
              return {
                outRef: `${transaction.txHash}#${index.toString()}`,
                outputIndex: index.toString(),
                output: makeWatcherL1PublicBytes(
                  output.to_canonical_cbor_hex(),
                ),
                datum:
                  datum === undefined
                    ? null
                    : {
                        datumHash: CML.hash_plutus_data(datum).to_hex(),
                        bytes: makeWatcherL1PublicBytes(
                          datum.to_canonical_cbor_hex(),
                        ),
                      },
                referenceScript:
                  (
                    transaction.utxos.find(
                      (candidate) =>
                        (candidate as MutableRecord).outputIndex ===
                        index.toString(),
                    ) as MutableRecord | undefined
                  )?.referenceScript ?? null,
              };
            },
          );
          return {
            ...transaction,
            transactionIndex: "0",
            fullTransaction: makeWatcherL1PublicBytes(
              fullTransaction.to_canonical_cbor_hex(),
            ),
            witnessSet: makeWatcherL1PublicBytes(
              witnessSet.to_canonical_cbor_hex(),
            ),
            utxos: transactionIsValid ? appliedUtxos : [],
            redeemers: canonicalRedeemers,
          };
        })();
  const l1Observation = {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
    network: "Preprod",
    providerId: authenticatedProvider.providerId,
    chainPoint:
      pointOverride ??
      ({
        blockHash: h32((40 + serial).toString(16).padStart(2, "0")),
        parentBlockHash: parentHash,
        slot: (1_000 + serial).toString(),
        blockNo: blockNo.toString(),
        depth: depth.toString(),
      } as const),
    transactions: canonicalTransaction === null ? [] : [canonicalTransaction],
  };
  const normalized = normalizeWatcherL1Block(
    authenticatedProvider,
    l1Observation,
  );
  const finalityObservations = [
    { authenticatedProvider, l1Observation },
    {
      authenticatedProvider: providerB,
      l1Observation: {
        ...structuredClone(l1Observation),
        providerId: providerB.providerId,
      },
    },
  ];
  const normalizedEvidence = finalityObservations.map(
    ({
      authenticatedProvider: authorityProvider,
      l1Observation: observation,
    }) => normalizeWatcherL1Block(authorityProvider, observation),
  );
  const consistency = evaluateWatcherMultiProviderConsistency(
    externalSource,
    normalizedEvidence,
  );
  const previousStateDigest =
    previousFinalityState !== null &&
    typeof previousFinalityState === "object" &&
    "stateDigest" in previousFinalityState &&
    typeof previousFinalityState.stateDigest === "string"
      ? previousFinalityState.stateDigest
      : null;
  const lineage =
    previousStateDigest === null
      ? []
      : (userEventFinalityLineageByStateDigest.get(previousStateDigest) ?? []);
  const finalityResult = evaluateWatcherFinality(
    selectedFinalityPolicy,
    previousFinalityState,
    consistency,
  );
  expect([
    "observe_pending",
    "advance_pending",
    "finalize",
    "duplicate",
    "rewind_pending",
  ]).toContain(finalityResult.action);
  if (finalityResult.state !== null) {
    userEventFinalityLineageByStateDigest.set(
      finalityResult.state.stateDigest,
      [
        ...lineage,
        {
          observations: finalityObservations,
          consistency,
          result: finalityResult,
        },
      ],
    );
  }
  const chainPoints = [
    ...sourceStore.chainPoints.filter(
      ({ chainPointId }) => chainPointId !== normalized.chainPoint.chainPointId,
    ),
    {
      chainPointId: normalized.chainPoint.chainPointId,
      providerId: normalized.provider.providerId,
      blockHash: normalized.chainPoint.blockHash,
      slot: normalized.chainPoint.slot,
      blockNo: normalized.chainPoint.blockNo,
      depth: normalized.chainPoint.depth,
    },
  ];
  const protocolUtxos = [
    ...new Map(
      [
        ...sourceStore.protocolUtxos.filter(
          ({ role }) =>
            !["deposit", "withdrawal", "forced_transaction"].includes(role),
        ),
        ...nextProtocolUtxos.map((utxo) => ({
          ...utxo,
          chainPointId:
            utxo.chainPointId === ""
              ? normalized.chainPoint.chainPointId
              : utxo.chainPointId,
        })),
      ].map((utxo) => [utxo.outRef, utxo]),
    ).values(),
  ];
  const protocolJournal = journalWatcherProtocolUtxoTransition({
    sourceStore,
    nextChainPoints: chainPoints,
    nextProtocolUtxos: protocolUtxos,
    spentAtChainPointId: normalized.chainPoint.chainPointId,
  });
  const store = makeWatcherDurableStore({
    deploymentMarker: policy.deploymentMarker,
    revision: (BigInt(sourceStore.revision) + 1n).toString(),
    records: {
      l1Observations: [
        ...sourceStore.l1Observations.filter(
          ({ observationId }) => observationId !== normalized.observationDigest,
        ),
        {
          observationId: normalized.observationDigest,
          providerId: normalized.provider.providerId,
          chainPointId: normalized.chainPoint.chainPointId,
          payload: makeWatcherDurablePayload(
            encodeWatcherNormalizedL1Block(normalized).toString("hex"),
          ),
        },
      ],
      chainPoints,
      ...protocolJournal,
      daProofInputs: sourceStore.daProofInputs,
      reconstructedStates: sourceStore.reconstructedStates,
      decisions: sourceStore.decisions,
      faults: sourceStore.faults,
      submissions: sourceStore.submissions,
      confirmations: sourceStore.confirmations,
      retries: sourceStore.retries,
      deadlines: sourceStore.deadlines,
      correctionResults: sourceStore.correctionResults,
    },
  });
  return {
    context: asWireValue({
      schemaVersion: WATCHER_USER_EVENT_PUBLIC_CONTEXT_SCHEMA_VERSION,
      authenticatedProvider,
      l1Observation,
      sourceDurableStore: sourceStore,
      durableStore: store,
      deploymentAuthority,
      rollbackRestoredEventUtxos: [],
      finalityAuthority: {
        policy: selectedFinalityPolicy,
        lineage,
        previousState: previousFinalityState,
        observations: finalityObservations,
        consistency,
        result: finalityResult,
      },
      rollbackAuthority: null,
    }),
    store,
    transactionHash: transaction?.txHash ?? null,
    finalityState: finalityResult.state,
  };
};

let serial = 0;
const blockBundle = (
  eventFixtures: readonly EventFixture[],
  priorStore: ReturnType<typeof makeWatcherDurableStore> | null = null,
  blockNo = 100,
  depth = 1,
  mintPurpose: "mint" | "spend" = "mint",
  mutateRedeemers?: (redeemers: MutableRecord[]) => void,
  transactionIsValid = true,
): BlockBundle => {
  let transaction:
    | {
        txHash: string;
        body: ReturnType<typeof makeWatcherL1PublicBytes>;
        utxos: readonly unknown[];
        scripts: readonly never[];
        datums: readonly never[];
        redeemers: readonly unknown[];
      }
    | undefined;
  if (eventFixtures.length > 0) {
    const inputs = CML.TransactionInputList.new();
    const outputs = CML.TransactionOutputList.new();
    const mint = CML.Mint.new();
    const certificates = CML.CertificateList.new();
    const utxos: unknown[] = [];
    const mintRedeemers: MutableRecord[] = [];
    const certificateRedeemers: MutableRecord[] = [];
    eventFixtures.forEach((fixture, index) => {
      inputs.add(fixture.nonceInput);
      outputs.add(fixture.output);
      mint.set(
        CML.ScriptHash.from_hex(fixture.fields.policyId),
        CML.AssetName.from_hex(fixture.assetNameHex),
        1n,
      );
      certificates.add(fixture.certificate);
      mintRedeemers.push({
        purpose: mintPurpose,
        index: index.toString(),
        bytes: makeWatcherL1PublicBytes(
          encodeUserEventMintRedeemerFor(
            fixture.kind,
            {
              AuthenticateEvent: {
                nonce_input_index: BigInt(index),
                event_output_index: BigInt(index),
                hub_ref_input_index: 0n,
                witness_registration_redeemer_index: BigInt(
                  eventFixtures.length + index,
                ),
              },
            },
            fixture.materialCarriage,
          ),
        ),
      });
      certificateRedeemers.push({
        purpose: "certificate",
        index: index.toString(),
        bytes: makeWatcherL1PublicBytes(fixture.certificateRedeemerHex),
      });
    });
    const redeemers = [...mintRedeemers, ...certificateRedeemers];
    mutateRedeemers?.(redeemers);
    const body = CML.TransactionBody.new(
      inputs,
      outputs,
      200_000n + BigInt(blockNo),
    );
    body.set_mint(mint);
    body.set_certs(certificates);
    const referenceInputs = CML.TransactionInputList.new();
    referenceInputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(HUB_OUT_REF.split("#")[0]!),
        0n,
      ),
    );
    for (const outRef of new Set(
      eventFixtures.flatMap(
        ({ extraReferenceOutRefs }) => extraReferenceOutRefs,
      ),
    )) {
      const [transactionId, outputIndex] = outRef.split("#");
      referenceInputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(transactionId!),
          BigInt(outputIndex!),
        ),
      );
    }
    body.set_reference_inputs(referenceInputs);
    body.set_ttl(1_000n);
    body.set_script_data_hash(USER_EVENT_SCRIPT_DATA_HASH);
    const bodyHex = body.to_canonical_cbor_hex();
    const mintPolicies = CML.TransactionBody.from_cbor_hex(bodyHex)
      .mint()!
      .keys();
    eventFixtures.forEach((fixture, eventIndex) => {
      let index = -1;
      for (
        let policyIndex = 0;
        policyIndex < mintPolicies.len();
        policyIndex += 1
      ) {
        if (
          mintPolicies.get(policyIndex).to_hex() === fixture.fields.policyId
        ) {
          index = policyIndex;
          break;
        }
      }
      if (index < 0) {
        throw new Error("missing event mint policy");
      }
      mintRedeemers[eventIndex]!.index = index.toString();
    });
    const txHash = computeHash32(Buffer.from(bodyHex, "hex")).toString("hex");
    eventFixtures.forEach((fixture, index) => {
      const datum = CML.PlutusData.from_cbor_hex(fixture.datumCborHex);
      utxos.push({
        outRef: `${txHash}#${index.toString()}`,
        outputIndex: index.toString(),
        output: makeWatcherL1PublicBytes(fixture.outputCborHex),
        datum: {
          datumHash: CML.hash_plutus_data(datum).to_hex(),
          bytes: makeWatcherL1PublicBytes(fixture.datumCborHex),
        },
        referenceScript: null,
      });
    });
    transaction = {
      txHash,
      body: makeWatcherL1PublicBytes(bodyHex),
      utxos,
      scripts: [],
      datums: [],
      redeemers,
    };
  }
  if (transaction === undefined) {
    return contextFromTransaction(
      null,
      priorStore,
      priorStore?.protocolUtxos ?? [],
      blockNo,
      depth,
      null,
      undefined,
    );
  }
  const createdProtocolUtxos: WatcherProtocolUtxo[] = eventFixtures.map(
    (fixture, index) => ({
      outRef: `${transaction.txHash}#${index.toString()}`,
      role:
        fixture.kind === "forced_order" ? "forced_transaction" : fixture.kind,
      chainPointId: "",
      output: makeWatcherDurablePayload(fixture.outputCborHex),
    }),
  );
  return contextFromTransaction(
    transaction,
    priorStore,
    transactionIsValid
      ? [...(priorStore?.protocolUtxos ?? []), ...createdProtocolUtxos]
      : (priorStore?.protocolUtxos ?? []),
    blockNo,
    depth,
    null,
    undefined,
    transactionIsValid,
  );
};

const depositSpendBundle = (
  state: WatcherUserEventIndexerState,
  priorStore: ReturnType<typeof makeWatcherDurableStore>,
  mutateBurn = false,
  mutateRedeemers?: (redeemers: MutableRecord[]) => void,
): BlockBundle => {
  const event = state.snapshot.activeEvents[0]!;
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(event.transactionHash),
      BigInt(event.outputIndex),
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(scriptAddress(applied.reserveSpend!)),
      CML.Value.from_coin(3_000_000n),
    ),
  );
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(event.policyId),
    CML.AssetName.from_hex(event.assetNameHex),
    mutateBurn ? -2n : -1n,
  );
  const certificates = CML.CertificateList.new();
  certificates.add(
    CML.Certificate.new_unreg_cert(
      CML.Credential.new_script(
        CML.ScriptHash.from_hex(event.witnessScriptHash),
      ),
      0n,
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_mint(mint);
  body.set_certs(certificates);
  const referenceInputs = CML.TransactionInputList.new();
  referenceInputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(h32("a0")), 0n),
  );
  referenceInputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(h32("a3")), 0n),
  );
  body.set_reference_inputs(referenceInputs);
  body.set_script_data_hash(USER_EVENT_SCRIPT_DATA_HASH);
  const bodyHex = body.to_canonical_cbor_hex();
  const txHash = computeHash32(Buffer.from(bodyHex, "hex")).toString("hex");
  const eventData = CML.PlutusData.from_cbor_hex(event.eventCborHex)
    .as_constr_plutus_data()!
    .fields();
  const rawProof = {
    domain: "DepositsRootDomain",
    root: settlementDatum.deposits_root,
    phas_root: MEMBERSHIP_PHAS_ROOT,
    count: 1n,
    key: eventData.get(0).to_cbor_hex(),
    value: eventData.get(1).to_cbor_hex(),
    proof: [],
  };
  const spendRedeemer = encodeData(
    {
      input_index: 0n,
      output_index: 0n,
      hub_ref_input_index: 0n,
      settlement_ref_input_index: 1n,
      mint_redeemer_index: 1n,
      membership_proof: rawProof,
      inclusion_proof_script_withdraw_redeemer_index: 3n,
    },
    DepositSpendRedeemer,
  );
  const burnRedeemer = encodeData(
    {
      BurnEventNFT: {
        nonce_asset_name: event.assetNameHex,
        witness_unregistration_redeemer_index: 2n,
      },
    },
    UserEventMintRedeemer,
  );
  const certificateRedeemer = encodeData(
    { MintOrBurn: { targetPolicy: event.policyId } },
    UserEventWitnessPublishRedeemer,
  );
  const membershipItems = CML.PlutusDataList.new();
  membershipItems.add(
    CML.PlutusData.from_cbor_hex(
      Data.to(MEMBERSHIP_PHAS_ROOT as never, MerkleRoot as never),
    ),
  );
  membershipItems.add(
    CML.PlutusData.new_bytes(Buffer.from(rawProof.key, "hex")),
  );
  membershipItems.add(
    CML.PlutusData.new_bytes(Buffer.from(rawProof.value, "hex")),
  );
  membershipItems.add(
    CML.PlutusData.from_cbor_hex(Data.to([] as never, Proof as never)),
  );
  const membershipRedeemer =
    CML.PlutusData.new_list(membershipItems).to_cbor_hex();
  const redeemers: MutableRecord[] = [
    {
      purpose: "spend",
      index: "0",
      bytes: makeWatcherL1PublicBytes(spendRedeemer),
    },
    {
      purpose: "mint",
      index: "0",
      bytes: makeWatcherL1PublicBytes(burnRedeemer),
    },
    {
      purpose: "certificate",
      index: "0",
      bytes: makeWatcherL1PublicBytes(certificateRedeemer),
    },
    {
      purpose: "withdrawal",
      index: "0",
      bytes: makeWatcherL1PublicBytes(membershipRedeemer),
    },
  ];
  mutateRedeemers?.(redeemers);
  return contextFromTransaction(
    {
      txHash,
      body: makeWatcherL1PublicBytes(bodyHex),
      utxos: [],
      scripts: [],
      datums: [],
      redeemers,
    },
    priorStore,
    [],
    101,
    1,
  );
};

const nonDepositSpendBundle = (
  state: WatcherUserEventIndexerState,
  priorStore: ReturnType<typeof makeWatcherDurableStore>,
  mutateRedeemers?: (redeemers: MutableRecord[]) => void,
): BlockBundle => {
  const event = state.snapshot.activeEvents[0]!;
  if (event.kind === "deposit") {
    throw new Error(
      "non-deposit terminal fixture requires a non-deposit event",
    );
  }
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(event.transactionHash),
      BigInt(event.outputIndex),
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(event.policyId),
    CML.AssetName.from_hex(event.assetNameHex),
    -1n,
  );
  if (event.kind === "withdrawal") {
    const payoutAssets = CML.MultiAsset.new();
    payoutAssets.set(
      CML.ScriptHash.from_hex(applied.payoutMint!),
      CML.AssetName.from_hex(event.assetNameHex),
      1n,
    );
    mint.set(
      CML.ScriptHash.from_hex(applied.payoutMint!),
      CML.AssetName.from_hex(event.assetNameHex),
      1n,
    );
    const withdrawalDatum = Data.from(
      event.datumCborHex,
      WithdrawalOrderDatum,
    ) as {
      event: {
        info: {
          body: {
            l2_value: unknown;
            l1_address: unknown;
            l1_datum: unknown;
          };
        };
      };
    };
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_hex(scriptAddress(applied.payoutSpend!)),
        CML.Value.new(3_000_000n, payoutAssets),
        CML.DatumOption.new_datum(
          CML.PlutusData.from_cbor_hex(
            Data.to(
              {
                l2_value: withdrawalDatum.event.info.body.l2_value,
                l1_address: withdrawalDatum.event.info.body.l1_address,
                l1_datum: withdrawalDatum.event.info.body.l1_datum,
              } as never,
              PayoutDatum,
            ),
          ),
        ),
      ),
    );
  } else {
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_hex(`60${h28("88")}`),
        CML.Value.from_coin(3_000_000n),
      ),
    );
  }
  const certificates = CML.CertificateList.new();
  certificates.add(
    CML.Certificate.new_unreg_cert(
      CML.Credential.new_script(
        CML.ScriptHash.from_hex(event.witnessScriptHash),
      ),
      0n,
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_mint(mint);
  body.set_certs(certificates);
  const referenceInputs = CML.TransactionInputList.new();
  referenceInputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(h32("a0")), 0n),
  );
  referenceInputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(h32("a3")), 0n),
  );
  body.set_reference_inputs(referenceInputs);
  body.set_script_data_hash(USER_EVENT_SCRIPT_DATA_HASH);
  const bodyHex = body.to_canonical_cbor_hex();
  const txHash = computeHash32(Buffer.from(bodyHex, "hex")).toString("hex");
  const eventFieldsData = CML.PlutusData.from_cbor_hex(event.eventCborHex)
    .as_constr_plutus_data()!
    .fields();
  const datum = Data.from(
    event.datumCborHex,
    event.kind === "withdrawal" ? WithdrawalOrderDatum : TxOrderDatum,
  ) as {
    event: {
      tx?: {
        tx_id: string;
        transaction_commitment: string;
        source: typeof emptyNativePayload.source;
      };
    };
  };
  const validity = "ForcedTxValid" as const;
  const value =
    event.kind === "withdrawal"
      ? eventFieldsData.get(1).to_cbor_hex()
      : Data.to(
          {
            tx_id: datum.event.tx!.tx_id,
            source: datum.event.tx!.source,
            verdict: validity,
          },
          ForcedInclusionTx,
        );
  const domain =
    event.kind === "withdrawal"
      ? "WithdrawalsRootDomain"
      : "ForcedTransactionsV1RootDomain";
  const rawProof = {
    domain,
    root:
      event.kind === "withdrawal"
        ? settlementDatum.withdrawals_root
        : settlementDatum.forced_transactions_root,
    phas_root: MEMBERSHIP_PHAS_ROOT,
    count: 1n,
    key: eventFieldsData.get(0).to_cbor_hex(),
    value,
    proof: [],
  };
  const canonicalMint = CML.TransactionBody.from_cbor_hex(bodyHex).mint()!;
  const eventMintPolicyIndex = [
    ...Array(canonicalMint.keys().len()).keys(),
  ].find(
    (index) => canonicalMint.keys().get(index).to_hex() === event.policyId,
  )!;
  const payoutMintPolicyIndex =
    event.kind === "withdrawal"
      ? [...Array(canonicalMint.keys().len()).keys()].find(
          (index) =>
            canonicalMint.keys().get(index).to_hex() === applied.payoutMint,
        )!
      : -1;
  const mintIndices = [eventMintPolicyIndex, payoutMintPolicyIndex]
    .filter((index) => index >= 0)
    .sort((left, right) => left - right);
  const eventBurnGlobalIndex = 1 + mintIndices.indexOf(eventMintPolicyIndex);
  const payoutMintGlobalIndex =
    event.kind === "withdrawal"
      ? 1 + mintIndices.indexOf(payoutMintPolicyIndex)
      : -1;
  const certificateGlobalIndex = 1 + mintIndices.length;
  const membershipGlobalIndex = certificateGlobalIndex + 1;
  const spendRedeemer =
    event.kind === "withdrawal"
      ? encodeData(
          {
            input_index: 0n,
            output_index: 0n,
            hub_ref_input_index: 0n,
            settlement_ref_input_index: 1n,
            burn_redeemer_index: BigInt(eventBurnGlobalIndex),
            payout_mint_redeemer_index: BigInt(payoutMintGlobalIndex),
            membership_proof: rawProof,
            inclusion_proof_script_withdraw_redeemer_index: BigInt(
              membershipGlobalIndex,
            ),
            purpose: "InitializePayout",
          },
          WithdrawalSpendRedeemer,
        )
      : encodeData(
          {
            input_index: 0n,
            output_index: 0n,
            hub_ref_input_index: 0n,
            settlement_ref_input_index: 1n,
            burn_redeemer_index: BigInt(eventBurnGlobalIndex),
            membership_proof: rawProof,
            inclusion_proof_script_withdraw_redeemer_index: BigInt(
              membershipGlobalIndex,
            ),
            validity_override: validity,
          },
          TxOrderSpendRedeemer,
        );
  // §8.11: a burn reads no material, so the tx-order policy requires its vector
  // to be empty, which the helper's default supplies.
  const burnRedeemer = encodeUserEventMintRedeemerFor(event.kind, {
    BurnEventNFT: {
      nonce_asset_name: event.assetNameHex,
      witness_unregistration_redeemer_index: BigInt(certificateGlobalIndex),
    },
  });
  const certificateRedeemer = encodeData(
    { MintOrBurn: { targetPolicy: event.policyId } },
    UserEventWitnessPublishRedeemer,
  );
  const membershipItems = CML.PlutusDataList.new();
  membershipItems.add(
    CML.PlutusData.from_cbor_hex(
      Data.to(MEMBERSHIP_PHAS_ROOT as never, MerkleRoot as never),
    ),
  );
  membershipItems.add(
    CML.PlutusData.new_bytes(Buffer.from(rawProof.key, "hex")),
  );
  membershipItems.add(
    CML.PlutusData.new_bytes(Buffer.from(rawProof.value, "hex")),
  );
  membershipItems.add(
    CML.PlutusData.from_cbor_hex(Data.to([] as never, Proof as never)),
  );
  const redeemers: MutableRecord[] = [
    {
      purpose: "spend",
      index: "0",
      bytes: makeWatcherL1PublicBytes(spendRedeemer),
    },
    {
      purpose: "mint",
      index: eventMintPolicyIndex.toString(),
      bytes: makeWatcherL1PublicBytes(burnRedeemer),
    },
  ];
  if (event.kind === "withdrawal") {
    redeemers.push({
      purpose: "mint",
      index: payoutMintPolicyIndex.toString(),
      bytes: makeWatcherL1PublicBytes(
        encodeData(
          {
            MintPayout: {
              withdrawal_utxo_out_ref: {
                transactionId: event.transactionHash,
                outputIndex: BigInt(event.outputIndex),
              },
              withdrawal_input_index: 0n,
              withdrawal_spend_redeemer_index: 0n,
              hub_ref_input_index: 0n,
            },
          },
          PayoutMintRedeemer,
        ),
      ),
    });
  }
  redeemers.push(
    {
      purpose: "certificate",
      index: "0",
      bytes: makeWatcherL1PublicBytes(certificateRedeemer),
    },
    {
      purpose: "withdrawal",
      index: "0",
      bytes: makeWatcherL1PublicBytes(
        CML.PlutusData.new_list(membershipItems).to_cbor_hex(),
      ),
    },
  );
  mutateRedeemers?.(redeemers);
  return contextFromTransaction(
    {
      txHash,
      body: makeWatcherL1PublicBytes(bodyHex),
      utxos: [],
      scripts: [],
      datums: [],
      redeemers,
    },
    priorStore,
    [],
    101,
    1,
  );
};

type RollbackSourceRecordOverrides = Partial<
  Pick<
    WatcherDurableStore,
    "protocolUtxos" | "spentProtocolUtxos" | "daProofInputs"
  >
>;

const rebuildRollbackSourceStore = (
  sourceStore: WatcherDurableStore,
  overrides: RollbackSourceRecordOverrides,
  revisionOverride?: string,
): WatcherDurableStore =>
  makeWatcherDurableStore({
    deploymentMarker: sourceStore.deploymentMarker,
    revision:
      revisionOverride ?? (BigInt(sourceStore.revision) + 1n).toString(),
    records: {
      l1Observations: sourceStore.l1Observations,
      chainPoints: sourceStore.chainPoints,
      protocolUtxos: overrides.protocolUtxos ?? sourceStore.protocolUtxos,
      spentProtocolUtxos:
        overrides.spentProtocolUtxos ?? sourceStore.spentProtocolUtxos,
      daProofInputs: overrides.daProofInputs ?? sourceStore.daProofInputs,
      reconstructedStates: sourceStore.reconstructedStates,
      decisions: sourceStore.decisions,
      faults: sourceStore.faults,
      submissions: sourceStore.submissions,
      confirmations: sourceStore.confirmations,
      retries: sourceStore.retries,
      deadlines: sourceStore.deadlines,
      correctionResults: sourceStore.correctionResults,
    },
  });

const rollbackBundle = (
  created: BlockBundle,
  restoredEventUtxos: readonly WatcherProtocolUtxo[] = [],
  sourceStoreTransform: (
    sourceStore: WatcherDurableStore,
  ) => WatcherDurableStore = (sourceStore) => sourceStore,
  replacementPointOverride?: Readonly<{
    blockHash: string;
    parentBlockHash: string | null;
    slot: string;
    blockNo: string;
    depth: string;
  }>,
): Readonly<{
  context: WatcherUserEventPublicContext;
  applied: ReturnType<typeof evaluateWatcherRollback>;
}> => {
  const oldRaw = created.context.l1Observation as Record<string, any>;
  const oldA = normalizeWatcherL1Block(provider, oldRaw);
  const oldRawB = {
    ...structuredClone(oldRaw),
    providerId: "provider-b",
  };
  const oldB = normalizeWatcherL1Block(providerB, oldRawB);
  const oldConsistency = evaluateWatcherMultiProviderConsistency(
    externalSource,
    [oldA, oldB],
  );
  const previousFinality = evaluateWatcherFinality(
    finalityPolicy,
    null,
    oldConsistency,
  ).state!;
  const replacementPoint = replacementPointOverride ?? {
    blockHash: h32("ee"),
    parentBlockHash: oldA.chainPoint.blockHash,
    slot: (BigInt(oldA.chainPoint.slot) + 1n).toString(),
    blockNo: (BigInt(oldA.chainPoint.blockNo) + 1n).toString(),
    depth: "1",
  };
  const replacementRawA = {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
    network: "Preprod",
    providerId: "provider-a",
    chainPoint: replacementPoint,
    transactions: [],
  } as const;
  const replacementRawB = {
    ...replacementRawA,
    providerId: "provider-b",
  } as const;
  const replacementA = normalizeWatcherL1Block(provider, replacementRawA);
  const replacementB = normalizeWatcherL1Block(providerB, replacementRawB);
  const replacementConsistency = evaluateWatcherMultiProviderConsistency(
    externalSource,
    [replacementA, replacementB],
  );
  const finalityResult = evaluateWatcherFinality(
    finalityPolicy,
    previousFinality,
    replacementConsistency,
  );
  expect(finalityResult.action).toBe("rewind_pending");

  const priorStore = created.store;
  const extraObservations = [oldB, replacementA, replacementB];
  const l1Observations = [
    ...priorStore.l1Observations,
    ...extraObservations.map((block) => ({
      observationId: block.observationDigest,
      providerId: block.provider.providerId,
      chainPointId: block.chainPoint.chainPointId,
      payload: makeWatcherDurablePayload(
        encodeWatcherNormalizedL1Block(block).toString("hex"),
      ),
    })),
  ];
  const chainPoints = [
    ...new Map(
      [
        ...priorStore.chainPoints,
        ...extraObservations.map((block) => ({
          chainPointId: block.chainPoint.chainPointId,
          providerId: block.provider.providerId,
          blockHash: block.chainPoint.blockHash,
          slot: block.chainPoint.slot,
          blockNo: block.chainPoint.blockNo,
          depth: block.chainPoint.depth,
        })),
      ].map((point) => [point.chainPointId, point]),
    ).values(),
  ];
  const sourceStore = sourceStoreTransform(
    makeWatcherDurableStore({
      deploymentMarker: policy.deploymentMarker,
      revision: "20",
      records: {
        l1Observations,
        chainPoints,
        protocolUtxos: priorStore.protocolUtxos,
        spentProtocolUtxos: priorStore.spentProtocolUtxos,
        daProofInputs: priorStore.daProofInputs,
        reconstructedStates: priorStore.reconstructedStates,
        decisions: priorStore.decisions,
        faults: priorStore.faults,
        submissions: priorStore.submissions,
        confirmations: priorStore.confirmations,
        retries: priorStore.retries,
        deadlines: priorStore.deadlines,
        correctionResults: priorStore.correctionResults,
      },
    }),
  );
  const rollbackBootstrap = makeWatcherRollbackBootstrapState(
    finalityPolicy,
    sourceStore,
    previousFinality,
  )!;
  const applied = evaluateWatcherRollback(
    finalityPolicy,
    sourceStore,
    previousFinality,
    replacementConsistency,
    finalityResult,
    rollbackBootstrap,
    rollbackBootstrap,
  );
  expect(applied.action).toBe("apply_rewind");
  expect(applied.nextStore).not.toBeNull();
  const verificationContext = {
    policy: finalityPolicy,
    sourceStore,
    previousFinalityState: previousFinality,
    consistency: replacementConsistency,
    finalityResult,
    previousRollbackState: rollbackBootstrap,
    rollbackBootstrapState: rollbackBootstrap,
  };
  return {
    context: asWireValue({
      schemaVersion: WATCHER_USER_EVENT_PUBLIC_CONTEXT_SCHEMA_VERSION,
      authenticatedProvider: null,
      l1Observation: null,
      sourceDurableStore: sourceStore,
      durableStore: applied.nextStore,
      deploymentAuthority,
      rollbackRestoredEventUtxos: restoredEventUtxos,
      finalityAuthority: null,
      rollbackAuthority: {
        result: applied,
        context: verificationContext,
      },
    }),
    applied,
  };
};

const postFinalityRecoveryEvidence = (
  rawObservation: Record<string, any>,
  depth: string,
) => {
  const chainPoint = {
    ...(structuredClone(rawObservation.chainPoint) as Record<string, unknown>),
    depth,
  };
  const primaryRaw = {
    ...structuredClone(rawObservation),
    providerId: provider.providerId,
    chainPoint,
  };
  const observations = [
    normalizeWatcherL1Block(provider, primaryRaw),
    normalizeWatcherL1Block(providerB, {
      ...structuredClone(primaryRaw),
      providerId: providerB.providerId,
    }),
  ];
  const consistency = evaluateWatcherMultiProviderConsistency(
    externalSource,
    observations,
  );
  expect(consistency).toMatchObject({
    status: "agreed",
    sourceMode: "external_providers",
    independentProviderCount: 2,
  });
  return { observations, consistency };
};

const postFinalityUserEventRecoveryBundle = (
  commonBundle: BlockBundle,
  orphanBundle: BlockBundle,
) => {
  const selectedFinalityPolicy = finalityPolicy;
  const commonRaw = commonBundle.context.l1Observation as Record<string, any>;
  const orphanRaw = orphanBundle.context.l1Observation as Record<string, any>;
  const common = postFinalityRecoveryEvidence(commonRaw, "0");
  const orphanPending = postFinalityRecoveryEvidence(orphanRaw, "1");
  const orphanFinalized = postFinalityRecoveryEvidence(orphanRaw, "2");
  const replacementRaw = {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
    network: "Preprod",
    providerId: provider.providerId,
    chainPoint: {
      blockHash: h32("e9"),
      parentBlockHash: common.observations[0]!.chainPoint.blockHash,
      slot: (BigInt(common.observations[0]!.chainPoint.slot) + 1n).toString(),
      blockNo: (
        BigInt(common.observations[0]!.chainPoint.blockNo) + 1n
      ).toString(),
      depth: "0",
    },
    transactions: [],
  };
  const replacement = postFinalityRecoveryEvidence(replacementRaw, "0");
  const pending = evaluateWatcherFinality(
    selectedFinalityPolicy,
    null,
    orphanPending.consistency,
  );
  expect(pending.action).toBe("observe_pending");
  const finalized = evaluateWatcherFinality(
    selectedFinalityPolicy,
    pending.state,
    orphanFinalized.consistency,
  );
  expect(finalized.action).toBe("finalize");
  const contradiction = evaluateWatcherFinality(
    selectedFinalityPolicy,
    finalized.state,
    replacement.consistency,
  );
  expect(contradiction.action).toBe("quarantine_incident");

  const baseStore = orphanBundle.store;
  const persistedObservations = [
    ...common.observations,
    ...orphanPending.observations,
    ...orphanFinalized.observations,
    ...replacement.observations,
  ];
  const sourceStore = makeWatcherDurableStore({
    deploymentMarker: baseStore.deploymentMarker,
    revision: (BigInt(baseStore.revision) + 1n).toString(),
    records: {
      l1Observations: [
        ...new Map(
          [
            ...baseStore.l1Observations,
            ...persistedObservations.map((observation) => ({
              observationId: observation.observationDigest,
              providerId: observation.provider.providerId,
              chainPointId: observation.chainPoint.chainPointId,
              payload: makeWatcherDurablePayload(
                encodeWatcherNormalizedL1Block(observation).toString("hex"),
              ),
            })),
          ].map((entry) => [entry.observationId, entry]),
        ).values(),
      ],
      chainPoints: [
        ...new Map(
          [
            ...baseStore.chainPoints,
            ...persistedObservations.map((observation) => ({
              chainPointId: observation.chainPoint.chainPointId,
              providerId: observation.provider.providerId,
              blockHash: observation.chainPoint.blockHash,
              slot: observation.chainPoint.slot,
              blockNo: observation.chainPoint.blockNo,
              depth: observation.chainPoint.depth,
            })),
          ].map((entry) => [entry.chainPointId, entry]),
        ).values(),
      ],
      protocolUtxos: baseStore.protocolUtxos,
      spentProtocolUtxos: baseStore.spentProtocolUtxos,
      daProofInputs: baseStore.daProofInputs,
      reconstructedStates: baseStore.reconstructedStates,
      decisions: baseStore.decisions,
      faults: baseStore.faults,
      submissions: baseStore.submissions,
      confirmations: baseStore.confirmations,
      retries: baseStore.retries,
      deadlines: baseStore.deadlines,
      correctionResults: baseStore.correctionResults,
    },
  });
  const rollbackBootstrapState = makeWatcherRollbackBootstrapState(
    selectedFinalityPolicy,
    sourceStore,
    finalized.state,
  )!;
  const incident = evaluateWatcherRollback(
    selectedFinalityPolicy,
    sourceStore,
    finalized.state,
    replacement.consistency,
    contradiction,
    rollbackBootstrapState,
    rollbackBootstrapState,
  );
  expect(incident.action).toBe("quarantine_incident");
  expect(incident.nextStore).not.toBeNull();
  expect(incident.rollbackState?.incident).not.toBeNull();
  const recoveryInput: WatcherPostFinalityRecoveryInput = {
    policy: selectedFinalityPolicy,
    sourceStore: incident.nextStore,
    currentStore: incident.nextStore,
    quarantinedRollbackState: incident.rollbackState,
    rollbackBootstrapState,
    previousCanonicalPath: [common.consistency, orphanFinalized.consistency],
    replacementCanonicalPath: [common.consistency, replacement.consistency],
    previousRecoveryState: null,
  };
  const recovery = evaluateWatcherPostFinalityRecovery(recoveryInput);
  expect(recovery).toMatchObject({
    action: "rewind_and_replay",
    protocolDecision: "resume_replay",
    reasonCodes: ["recovery_applied"],
    recoveryState: {
      path: {
        commonAncestorPointDigest:
          common.observations[0]!.chainPoint.pointDigest,
        rollbackDepth: "1",
      },
      incidentLifecycle: { status: "recovered" },
    },
    resumableFinalityState: {
      phase: "unobserved",
      incident: null,
    },
  });
  const context: WatcherUserEventPublicContext = {
    schemaVersion: WATCHER_USER_EVENT_PUBLIC_CONTEXT_SCHEMA_VERSION,
    authenticatedProvider: null,
    l1Observation: null,
    sourceDurableStore: incident.nextStore,
    durableStore: recovery.nextStore,
    deploymentAuthority,
    rollbackRestoredEventUtxos: [],
    finalityAuthority: null,
    rollbackAuthority: {
      result: recovery,
      context: recoveryInput,
    },
  };
  return {
    context,
    recovery,
    recoveryInput,
    replacementObservationIds: replacement.observations.map(
      ({ observationDigest }) => observationDigest,
    ),
  };
};

const accepted = (
  previous: WatcherUserEventIndexerState | null,
  bundle: BlockBundle,
): WatcherUserEventIndexerState => {
  const observation = deriveWatcherUserEventObservation(
    policy,
    previous,
    bundle.context,
  );
  expect(observation).not.toBeNull();
  const indexed = evaluateWatcherUserEventIndexer(
    policy,
    previous,
    observation,
    bundle.context,
  );
  expect(indexed.action, JSON.stringify(indexed)).toBe("accept");
  expect(indexed.protocolDecision).toBe("indexed");
  expect(
    parseWatcherUserEventIndexerResult(JSON.parse(JSON.stringify(indexed)), {
      policy,
      previousState: previous,
      observation,
      publicContext: bundle.context,
    }),
  ).toEqual(indexed);
  expect(
    parseWatcherUserEventIndexerState(
      JSON.parse(JSON.stringify(indexed.state)),
      policy,
    ),
  ).toEqual(indexed.state);
  return indexed.state!;
};

describe("canonical authenticated user-event indexer", () => {
  it("accepts reordered wire keys while rejecting array, mutation, unknown, and unsupported changes", () => {
    const bundle = blockBundle(
      [makeEventFixture("deposit", "9a", 0, 1_000n)],
      null,
      100,
      1,
    );
    const observation = deriveWatcherUserEventObservation(
      policy,
      null,
      bundle.context,
    );
    expect(observation).not.toBeNull();
    const indexed = evaluateWatcherUserEventIndexer(
      policy,
      null,
      observation,
      bundle.context,
    );
    expect(indexed.action).toBe("accept");

    const context = {
      policy,
      previousState: null,
      observation,
      publicContext: bundle.context,
    };
    const reordered = reorderWireKeys(asWireValue(indexed));
    expect(parseWatcherUserEventIndexerResult(reordered, context)).toEqual(
      indexed,
    );
    expect(
      parseWatcherUserEventIndexerState(
        (reordered as Record<string, unknown>).state,
        policy,
      ),
    ).toEqual(indexed.state);

    expect(
      parseWatcherUserEventIndexerResult(
        new Proxy(asWireValue(indexed), {}),
        context,
      ),
    ).toBeNull();
    const nestedProxy = asWireValue(indexed) as Record<string, unknown>;
    const nestedState = nestedProxy.state as Record<string, unknown>;
    nestedState.snapshot = new Proxy(nestedState.snapshot as object, {});
    expect(parseWatcherUserEventIndexerResult(nestedProxy, context)).toBeNull();

    const arrayTampered = asWireValue(indexed) as Record<string, unknown>;
    arrayTampered.reasonCodes = [
      "duplicate_observation",
      ...(arrayTampered.reasonCodes as readonly string[]),
    ];
    expect(parseWatcherUserEventIndexerResult(arrayTampered, context)).toBe(
      null,
    );

    const mutated = asWireValue(indexed) as Record<string, unknown>;
    mutated.resultDigest = h32("ff");
    expect(parseWatcherUserEventIndexerResult(mutated, context)).toBe(null);

    const unknown = asWireValue(indexed) as Record<string, unknown>;
    unknown.unexpected = true;
    expect(parseWatcherUserEventIndexerResult(unknown, context)).toBe(null);

    for (const unsupportedValue of [1n, new Date(0)]) {
      const unsupported = asWireValue(indexed) as Record<string, unknown>;
      const state = unsupported.state as Record<string, unknown>;
      const history = state.history as Array<Record<string, unknown>>;
      const publicContext = history[0]!.publicContext as Record<
        string,
        unknown
      >;
      publicContext.authenticatedProvider = unsupportedValue;
      expect(parseWatcherUserEventIndexerResult(unsupported, context)).toBe(
        null,
      );
    }
  });

  it("rejects a proxied user-event result before canonical integrity comparison", () => {
    const bundle = blockBundle(
      [makeEventFixture("deposit", "9a", 0, 1_000n)],
      null,
      100,
      1,
    );
    const observation = deriveWatcherUserEventObservation(
      policy,
      null,
      bundle.context,
    );
    expect(observation).not.toBeNull();
    const indexed = evaluateWatcherUserEventIndexer(
      policy,
      null,
      observation,
      bundle.context,
    );
    expect(indexed.action).toBe("accept");

    expect(
      parseWatcherUserEventIndexerResult(new Proxy(asWireValue(indexed), {}), {
        policy,
        previousState: null,
        observation,
        publicContext: bundle.context,
      }),
    ).toBeNull();
  });

  it("requires one live, uniquely matching transport capability for every W10/W11 replay", async () => {
    const external = blockBundle(
      [makeEventFixture("deposit", "9a", 0, 1_000n)],
      null,
      100,
      1,
    );
    const observation = deriveWatcherUserEventObservation(
      policy,
      null,
      external.context,
    )!;
    const providerATransport = transportForProvider(provider);
    const providerBTransport = transportForProvider(providerB);

    expect(
      deriveWatcherUserEventObservationRaw(policy, null, external.context, []),
    ).toBeNull();
    expect(
      evaluateWatcherUserEventIndexerRaw(
        policy,
        null,
        observation,
        external.context,
        [],
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["public_evidence_mismatch"],
    });
    expect(
      deriveWatcherUserEventObservationRaw(
        policy,
        null,
        external.context,
        structuredClone(watcherTransportContexts),
      ),
    ).toBeNull();
    expect(
      deriveWatcherUserEventObservationRaw(policy, null, external.context, [
        providerBTransport,
      ]),
    ).toBeNull();
    expect(
      deriveWatcherUserEventObservationRaw(policy, null, external.context, [
        ...watcherTransportContexts,
        providerATransport,
      ]),
    ).toBeNull();
  });

  it("indexes exact deposit, withdrawal, and forced-order bytes with NFT, datum, witness, provenance, and finality", () => {
    expect(policy).not.toBeNull();
    const fixtures: EventFixture[] = [];
    for (const [index, kind] of (
      ["deposit", "withdrawal", "forced_order"] as const
    ).entries()) {
      const fixture = makeEventFixture(
        kind,
        (0xa1 + index).toString(16),
        index,
        1_000n,
      );
      fixtures.push(fixture);
      const state = accepted(null, blockBundle([fixture], null, 100, 1));
      expect(state.snapshot.activeEvents).toHaveLength(1);
      expect(state.snapshot.activeEvents[0]).toMatchObject({
        kind,
        policyId: fixture.fields.policyId,
        assetNameHex: fixture.assetNameHex,
        witnessScriptHash: userEventWitnessScriptHash(fixture.assetNameHex),
        inclusionTime: resolveEventInclusionTime(1_000, "Preprod").toString(),
        finalityStatus: "pending",
      });
      expect(state.snapshot.activeEvents[0]!.datumCborHex).toBe(
        fixture.datumCborHex,
      );
      expect(state.snapshot.activeEvents[0]!.outputCborHex).toBe(
        fixture.outputCborHex,
      );
    }
    const globalIndexState = accepted(
      null,
      blockBundle(fixtures, null, 101, 1),
    );
    expect(
      globalIndexState.snapshot.activeEvents.map(({ kind }) => kind),
    ).toEqual(["deposit", "withdrawal", "forced_order"]);
  });

  it.each(["external_providers"] as const)(
    "rejects %s two-step competing forks and oversized W12 evidence",
    () => {
      const initial = blockBundle(
        [makeEventFixture("deposit", "d3", 0, 1_000n)],
        null,
        100,
        1,
        "mint",
        undefined,
      );
      const state = accepted(null, initial);
      const oversized = structuredClone(initial.context) as MutableRecord;
      oversized.finalityAuthority.observations = Array.from(
        { length: 17 },
        () => structuredClone(oversized.finalityAuthority.observations[0]),
      );
      expect(
        deriveWatcherUserEventObservation(policy, null, oversized),
      ).toBeNull();
      const cyclic = structuredClone(initial.context) as MutableRecord;
      cyclic.finalityAuthority.lineage = [cyclic.finalityAuthority];
      expect(
        deriveWatcherUserEventObservation(policy, null, cyclic),
      ).toBeNull();
      const oversizedSparse = structuredClone(initial.context) as MutableRecord;
      const sparseLineage: unknown[] = [];
      sparseLineage.length =
        WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries + 1;
      oversizedSparse.finalityAuthority.lineage = sparseLineage;
      expect(
        deriveWatcherUserEventObservation(policy, null, oversizedSparse),
      ).toBeNull();
      const veryWide = structuredClone(initial.context) as MutableRecord;
      veryWide.untrusted = Object.fromEntries(
        Array.from(
          {
            length:
              WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries + 1,
          },
          (_, index) => [`field_${index.toString()}`, "x"],
        ),
      );
      expect(
        deriveWatcherUserEventObservation(policy, null, veryWide),
      ).toBeNull();

      const invalid = blockBundle(
        [makeEventFixture("deposit", "d7", 0, 1_000n)],
        null,
        100,
        1,
        "mint",
        undefined,
        false,
      );
      const invalidObservation = deriveWatcherUserEventObservation(
        policy,
        null,
        invalid.context,
      );
      expect(invalidObservation).not.toBeNull();
      expect(
        evaluateWatcherUserEventIndexer(
          policy,
          null,
          invalidObservation,
          invalid.context,
        ),
      ).toMatchObject({
        action: "accept",
        state: {
          snapshot: {
            activeEvents: [],
            terminalEvents: [],
          },
        },
      });

      const fork = blockBundle(
        [makeEventFixture("deposit", "d5", 0, 1_000n)],
        initial.store,
        102,
        1,
        "mint",
        undefined,
      );
      expect(
        deriveWatcherUserEventObservation(policy, state, fork.context),
      ).toBeNull();
    },
  );

  it.each(["external_providers"] as const)(
    "authenticates %s ancestry across an empty intermediate block",
    () => {
      const initial = blockBundle([], null, 100, 1, "mint", undefined);
      const state = accepted(null, initial);
      const gap = contextFromTransaction(
        null,
        initial.store,
        initial.store.protocolUtxos,
        101,
        1,
        null,
        undefined,
      );
      const gapPoint = (gap.context.l1Observation as MutableRecord)
        .chainPoint as MutableRecord;
      const current = contextFromTransaction(
        null,
        initial.store,
        initial.store.protocolUtxos,
        102,
        1,
        gap.finalityState,
        {
          blockHash: h32("e7"),
          parentBlockHash: String(gapPoint.blockHash),
          slot: (BigInt(String(gapPoint.slot)) + 1n).toString(),
          blockNo: (BigInt(String(gapPoint.blockNo)) + 1n).toString(),
          depth: "1",
        },
      );
      const observation = deriveWatcherUserEventObservation(
        policy,
        state,
        current.context,
      );
      expect(observation).not.toBeNull();
      expect(
        evaluateWatcherUserEventIndexer(
          policy,
          state,
          observation,
          current.context,
        ),
      ).toMatchObject({
        action: "accept",
        reasonCodes: ["block_authenticated"],
      });
    },
  );

  it("promotes pending status only at the release-bound depth and rejects omitted topology", () => {
    const fixture = makeEventFixture("deposit", "b1", 0, 1_000n);
    const firstBundle = blockBundle([fixture], null, 100, 1);
    const pendingState = accepted(null, firstBundle);
    const firstRaw = firstBundle.context.l1Observation as {
      transactions: readonly [
        {
          txHash: string;
          body: ReturnType<typeof makeWatcherL1PublicBytes>;
          utxos: readonly unknown[];
          scripts: readonly never[];
          datums: readonly never[];
          redeemers: readonly unknown[];
        },
      ];
      chainPoint: {
        blockHash: string;
        parentBlockHash: string | null;
        slot: string;
        blockNo: string;
        depth: string;
      };
    };
    const finalityBundle = contextFromTransaction(
      firstRaw.transactions[0],
      firstBundle.store,
      firstBundle.store.protocolUtxos,
      100,
      2,
      firstBundle.finalityState,
      { ...firstRaw.chainPoint, depth: "2" },
    );
    const finalState = accepted(pendingState, finalityBundle);
    expect(finalState.snapshot.activeEvents[0]?.finalityStatus).toBe("final");

    const omitted = blockBundle([], null, 103, 1);
    expect(
      deriveWatcherUserEventObservation(policy, finalState, omitted.context),
    ).toBeNull();
  });

  it("authenticates an exact spend/burn/witness-unregistration lifecycle and rejects adjacent burn quantity", () => {
    const fixture = makeEventFixture("deposit", "b2", 0, 1_000n);
    const created = blockBundle([fixture], null, 100, 1);
    const active = accepted(null, created);
    const terminal = accepted(
      active,
      depositSpendBundle(active, created.store),
    );
    expect(terminal.snapshot.activeEvents).toEqual([]);
    expect(terminal.snapshot.terminalEvents).toMatchObject([
      {
        kind: "deposit",
        terminalStatus: "absorbed",
      },
    ]);

    expect(
      deriveWatcherUserEventObservation(
        policy,
        active,
        depositSpendBundle(active, created.store, true).context,
      ),
    ).toBeNull();
  });

  it("authenticates exact withdrawal-payout and forced-order terminal semantics", () => {
    for (const [kind, terminalStatus] of [
      ["withdrawal", "payout_initialized"],
      ["forced_order", "processed"],
    ] as const) {
      const fixture = makeEventFixture(
        kind,
        kind === "withdrawal" ? "b4" : "b5",
        0,
        1_000n,
      );
      const created = blockBundle([fixture], null, 100, 1);
      const active = accepted(null, created);
      const terminal = accepted(
        active,
        nonDepositSpendBundle(active, created.store),
      );
      expect(terminal.snapshot.activeEvents).toEqual([]);
      expect(terminal.snapshot.terminalEvents).toMatchObject([
        {
          kind,
          terminalStatus,
          terminalFinalityStatus: "pending",
        },
      ]);
      if (kind === "forced_order") {
        // §8.11: a burn reads no material, so the tx-order policy requires its
        // carriage vector to be empty rather than ignore it — an unread wire field
        // is a second spelling of the same transaction (§6.1). Re-derived here,
        // and this is the refutation: the same burn with one entry in the vector
        // yields no observation.
        const carryingBurn = nonDepositSpendBundle(
          active,
          created.store,
          (redeemers) => {
            const burn = redeemers.find(
              (candidate) => candidate.purpose === "mint",
            )!;
            burn.bytes = makeWatcherL1PublicBytes(
              encodeUserEventMintRedeemerFor(
                "forced_order",
                (
                  Data.from(
                    (burn.bytes as { bytesHex: string }).bytesHex,
                    TxOrderMintRedeemer as never,
                  ) as { event: unknown }
                ).event,
                [{ Inline: { preimage: "80" } }],
              ),
            );
          },
        );
        expect(
          deriveWatcherUserEventObservation(
            policy,
            active,
            carryingBurn.context,
          ),
        ).toBeNull();
      }
      const terminalEvent = terminal.snapshot.terminalEvents[0]!;
      if (kind === "forced_order") {
        expect(terminalEvent.terminalClassification).toStrictEqual({
          schemaVersion: "midgard-watcher-forced-terminal-classification-v1",
          operatorValidity: "ForcedTxValid",
          terminalTransactionHash: terminalEvent.terminalTransactionHash,
          terminalPointDigest: terminalEvent.terminalPointDigest,
        });
        expect(Object.isFrozen(terminalEvent.terminalClassification)).toBe(
          true,
        );
        for (const mutate of [
          (candidate: Record<string, unknown>) => {
            delete candidate.terminalClassification;
          },
          (candidate: Record<string, unknown>) => {
            const classification = candidate.terminalClassification as Record<
              string,
              unknown
            >;
            classification.operatorValidity = "WrongVerdict";
          },
          (candidate: Record<string, unknown>) => {
            const classification = candidate.terminalClassification as Record<
              string,
              unknown
            >;
            classification.terminalTransactionHash = "00";
          },
          (candidate: Record<string, unknown>) => {
            const classification = candidate.terminalClassification as Record<
              string,
              unknown
            >;
            classification.extra = true;
          },
        ]) {
          const hostile = structuredClone(terminal) as unknown as Record<
            string,
            unknown
          >;
          const snapshot = hostile.snapshot as Record<string, unknown>;
          const events = snapshot.terminalEvents as Record<string, unknown>[];
          mutate(events[0]!);
          expect(parseWatcherUserEventIndexerState(hostile, policy)).toBeNull();
        }

        const hostileActive = structuredClone(active) as unknown as Record<
          string,
          unknown
        >;
        const activeSnapshot = hostileActive.snapshot as Record<
          string,
          unknown
        >;
        const activeEvents = activeSnapshot.activeEvents as Record<
          string,
          unknown
        >[];
        activeEvents[0]!.terminalClassification =
          terminalEvent.terminalClassification;
        expect(
          parseWatcherUserEventIndexerState(hostileActive, policy),
        ).toBeNull();
      } else {
        expect(terminalEvent).not.toHaveProperty("terminalClassification");
        const hostile = structuredClone(terminal) as unknown as Record<
          string,
          unknown
        >;
        const snapshot = hostile.snapshot as Record<string, unknown>;
        const events = snapshot.terminalEvents as Record<string, unknown>[];
        events[0]!.terminalClassification = Object.freeze({
          schemaVersion: "midgard-watcher-forced-terminal-classification-v1",
          operatorValidity: "ForcedTxValid",
          terminalTransactionHash: terminalEvent.terminalTransactionHash,
          terminalPointDigest: terminalEvent.terminalPointDigest,
        });
        expect(parseWatcherUserEventIndexerState(hostile, policy)).toBeNull();
      }
    }
  });

  it("rejects cyclic, aliased, and cumulatively oversized raw evidence before hashing or replay", () => {
    const fixture = makeEventFixture("deposit", "ae", 0, 1_000n);
    const bundle = blockBundle([fixture]);
    const observation = deriveWatcherUserEventObservation(
      policy,
      null,
      bundle.context,
    )!;

    const cyclicObservation = asWireValue(observation) as Record<string, any>;
    cyclicObservation.snapshot.activeEvents = cyclicObservation;
    expect(
      evaluateWatcherUserEventIndexer(
        policy,
        null,
        cyclicObservation,
        bundle.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_observation"],
    });

    const aliasedContext = asWireValue(bundle.context) as Record<string, any>;
    aliasedContext.durableStore = aliasedContext.sourceDurableStore;
    expect(
      deriveWatcherUserEventObservation(policy, null, aliasedContext),
    ).toBeNull();

    const oversizedObservation = asWireValue(observation) as Record<
      string,
      any
    >;
    oversizedObservation.blockHash = "a".repeat(5 * 1_024 * 1_024);
    oversizedObservation.pointDigest = "b".repeat(5 * 1_024 * 1_024);
    expect(
      evaluateWatcherUserEventIndexer(
        policy,
        null,
        oversizedObservation,
        bundle.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_observation"],
    });

    const state = accepted(null, bundle);
    const cyclicState = JSON.parse(JSON.stringify(state)) as Record<
      string,
      any
    >;
    cyclicState.history[0].publicContext.sourceDurableStore = cyclicState;
    expect(parseWatcherUserEventIndexerState(cyclicState, policy)).toBeNull();
  });

  it("observes a material-bearing forced order on its §4 self-binding alone", () => {
    // This used to require an authenticated terminal receipt for non-empty
    // material: the bare fixture had to be rejected, and acceptance needed a
    // `TxFieldReceiptV1` UTxO seeded in the durable store, its minted asset name
    // re-derived, its `collection_proof` verified, and the order's
    // `terminal_receipt_reference` pointing at it as a third reference input.
    // #587 retired that chain — its mint policy was unsatisfiable under
    // `docs/spec/midgard-tx.md` §4 — and `TxOrderPayloadV1` shed the reference, so
    // there is no availability evidence in the datum left to check. What remains is
    // the §4 binding, and it is asserted on material-bearing material precisely
    // because that is where the retired walk used to be reached.
    //
    // The watcher does not re-derive `verify_order_material`'s temporary
    // all-empty clause; `forcedPayloadMatchesNativeSource`'s docstring records the
    // reasoning, and the next test is the material-bearing refusal that clause's
    // absence does *not* weaken. A forced order over the canonically-empty
    // transaction moves nothing, so refusing material here would leave the
    // watcher's forced-inclusion verification — `event-classification-verifier`'s
    // `OperatorVerdictV1` classification and `block-replay`'s `ForcedTransaction`
    // replay, both of which drive this same gate through
    // `tests/support/w15-authority-scenarios.ts` — with no reachable subject.
    const fixture = makeEventFixture(
      "forced_order",
      "b6",
      0,
      1_000n,
      0n,
      undefined,
      undefined,
      nonEmptyNativePayload,
      [],
      nonEmptyNativeCarriage,
    );
    const bundle = blockBundle([fixture], null, 100, 1);
    const observation = deriveWatcherUserEventObservation(
      policy,
      null,
      bundle.context,
    );
    expect(observation).not.toBeNull();
    expect(
      evaluateWatcherUserEventIndexer(
        policy,
        null,
        observation,
        bundle.context,
      ),
    ).toMatchObject({
      action: "accept",
      state: {
        snapshot: {
          activeEvents: [{ kind: "forced_order" }],
        },
      },
    });
  });

  it("refuses a tx-order mint redeemer in the retired bare-enum shape", () => {
    // #594 gave the tx-order minting policy its own `MintRedeemer` — the shared
    // `user_events.MintRedeemer` wrapped beside the §8 carriage vector — so the
    // wire form at that policy is `Constr 0 [<enum>, <list>]`. A redeemer in the
    // *old* bare-enum spelling is what the deployed policy will refuse once the
    // blueprint is regenerated, and it must not be indexed here either.
    //
    // The refusal is loud in this module's only sense of the word: no observation
    // for the containing block at all, which is the same taxonomy every other
    // malformed mint redeemer takes (`scanCreatedEvents` returns `null`, and the
    // caller turns that into a null observation). It is not a per-event drop —
    // there is no code path in this indexer that skips one event and keeps the
    // rest, and the negative below pins that a stale watcher against a new mint
    // stops rather than quietly shipping an incomplete active set.
    const fixture = makeEventFixture("forced_order", "b9", 0, 1_000n);
    const bareEnum = blockBundle(
      [fixture],
      null,
      100,
      1,
      "mint",
      (redeemers) => {
        redeemers[0]!.bytes = makeWatcherL1PublicBytes(
          encodeData(
            {
              AuthenticateEvent: {
                nonce_input_index: 0n,
                event_output_index: 0n,
                hub_ref_input_index: 0n,
                witness_registration_redeemer_index: 1n,
              },
            },
            UserEventMintRedeemer,
          ),
        );
      },
    );
    expect(
      deriveWatcherUserEventObservation(policy, null, bareEnum.context),
    ).toBeNull();
  });

  it("refuses a forced order whose carriage vector its nine commitments do not exhaust", () => {
    // §8.11's exhaustion rule, re-derived. A spare entry lets two distinct
    // redeemers spell one order, the second naming reference inputs nothing in the
    // mint's walk ever read; a short vector leaves a field's material uncarried.
    // Both directions are reachable from this module — the vector is in the
    // redeemer and the non-empty slot count is in the payload — so both are
    // checked, and the per-field hash half that is *not* reachable is documented
    // on `forcedPayloadMatchesNativeSource`.
    const spare = makeEventFixture(
      "forced_order",
      "ba",
      0,
      1_000n,
      0n,
      undefined,
      undefined,
      nonEmptyNativePayload,
      [],
      [...nonEmptyNativeCarriage, { Inline: { preimage: "80" } }],
    );
    expect(
      deriveWatcherUserEventObservation(
        policy,
        null,
        blockBundle([spare], null, 100, 1).context,
      ),
    ).toBeNull();

    const short = makeEventFixture(
      "forced_order",
      "bb",
      0,
      1_000n,
      0n,
      undefined,
      undefined,
      nonEmptyNativePayload,
      [],
      [],
    );
    expect(
      deriveWatcherUserEventObservation(
        policy,
        null,
        blockBundle([short], null, 100, 1).context,
      ),
    ).toBeNull();
  });

  it("rejects a material-bearing forced order whose carried commitment is not its source's", () => {
    // The material-bearing negative: the §4 binding is the whole of what the datum
    // claims now, so breaking it is the way a forced order carrying real material
    // fails to produce an observation.
    const fixture = makeEventFixture(
      "forced_order",
      "b7",
      0,
      1_000n,
      0n,
      undefined,
      undefined,
      {
        ...nonEmptyNativePayload,
        transaction_commitment: h32("de"),
      },
    );
    expect(
      deriveWatcherUserEventObservation(
        policy,
        null,
        blockBundle([fixture]).context,
      ),
    ).toBeNull();
  });

  it("promotes terminal finality independently and restores terminal consumption through W13 restart/reprocessing", () => {
    const finalityFixture = makeEventFixture("deposit", "b8", 0, 1_000n);
    const finalityCreated = blockBundle([finalityFixture], null, 100, 1);
    const finalityActive = accepted(null, finalityCreated);
    const pendingBundle = depositSpendBundle(
      finalityActive,
      finalityCreated.store,
    );
    const pending = accepted(finalityActive, pendingBundle);
    expect(pending.snapshot.terminalEvents[0]?.terminalFinalityStatus).toBe(
      "pending",
    );
    const rawPending = pendingBundle.context.l1Observation as MutableRecord;
    const pendingPoint = rawPending.chainPoint as {
      blockHash: string;
      parentBlockHash: string | null;
      slot: string;
      blockNo: string;
      depth: string;
    };
    const finalityBundle = contextFromTransaction(
      rawPending.transactions[0],
      pendingBundle.store,
      pendingBundle.store.protocolUtxos,
      Number(pendingPoint.blockNo),
      2,
      pendingBundle.finalityState,
      { ...pendingPoint, depth: "2" },
    );
    const finalized = accepted(pending, finalityBundle);
    expect(finalized.snapshot.terminalEvents[0]?.terminalFinalityStatus).toBe(
      "final",
    );

    const rollbackFixture = makeEventFixture("deposit", "b9", 0, 1_000n);
    const rollbackCreated = blockBundle([rollbackFixture], null, 100, 1);
    const rollbackActive = accepted(null, rollbackCreated);
    const terminalBundle = depositSpendBundle(
      rollbackActive,
      rollbackCreated.store,
    );
    const terminalState = accepted(rollbackActive, terminalBundle);
    const restoredUtxos = rollbackCreated.store.protocolUtxos.filter(
      ({ role }) => role === "deposit",
    );
    const rollback = rollbackBundle(terminalBundle, restoredUtxos);
    const targetDigest = rollbackActive.activeEntryDigests.at(-1)!;
    const observation = deriveWatcherUserEventObservation(
      policy,
      terminalState,
      rollback.context,
      targetDigest,
    );
    expect(observation).not.toBeNull();
    const rewound = evaluateWatcherUserEventIndexer(
      policy,
      terminalState,
      observation,
      rollback.context,
    );
    expect(rewound).toMatchObject({
      action: "accept",
      state: {
        snapshot: {
          activeEvents: [{ kind: "deposit" }],
          terminalEvents: [],
        },
      },
    });
    const restarted = parseWatcherUserEventIndexerState(
      JSON.parse(JSON.stringify(rewound.state)),
      policy,
    );
    expect(restarted).toEqual(rewound.state);
    const reprocessed = accepted(
      restarted,
      depositSpendBundle(restarted!, rollback.applied.nextStore!),
    );
    expect(reprocessed.snapshot.activeEvents).toEqual([]);
    expect(reprocessed.snapshot.terminalEvents).toMatchObject([
      { terminalStatus: "absorbed" },
    ]);
  });

  it("routes same-point content replacement through authenticated rollback before replay", () => {
    const olderBlock = blockBundle([], null, 89, 1);
    const olderState = accepted(null, olderBlock);
    const created = blockBundle(
      [makeEventFixture("deposit", "b2", 0, 1_000n)],
      olderBlock.store,
      90,
      1,
    );
    const active = accepted(olderState, created);
    const oldPoint = (created.context.l1Observation as MutableRecord)
      .chainPoint as {
      blockHash: string;
      parentBlockHash: string | null;
      slot: string;
      blockNo: string;
      depth: string;
    };
    const replacement = contextFromTransaction(
      null,
      created.store,
      created.store.protocolUtxos,
      Number(oldPoint.blockNo),
      Number(oldPoint.depth),
      created.finalityState,
      oldPoint,
    );
    expect(replacement.context.finalityAuthority?.result).toMatchObject({
      action: "rewind_pending",
      protocolDecision: "rewind_required",
      reasonCodes: ["pending_content_changed"],
    });
    expect(
      deriveWatcherUserEventObservation(policy, active, replacement.context),
    ).toBeNull();

    const rollback = rollbackBundle(
      created,
      [],
      (sourceStore) => sourceStore,
      oldPoint,
    );
    expect(rollback.applied.action).toBe("apply_rewind");
    const rollbackObservation = deriveWatcherUserEventObservation(
      policy,
      active,
      rollback.context,
      olderState.activeEntryDigests.at(-1)!,
    );
    expect(rollbackObservation?.transitionKind).toBe("rollback");
    const rewound = evaluateWatcherUserEventIndexer(
      policy,
      active,
      rollbackObservation,
      rollback.context,
    );
    expect(rewound).toMatchObject({
      action: "accept",
      reasonCodes: ["rollback_authenticated"],
      state: { snapshot: { activeEvents: [] } },
    });

    const replay = contextFromTransaction(
      null,
      rollback.applied.nextStore,
      rollback.applied.nextStore!.protocolUtxos,
      Number(oldPoint.blockNo),
      Number(oldPoint.depth),
      replacement.finalityState,
      oldPoint,
    );
    const replayObservation = deriveWatcherUserEventObservation(
      policy,
      rewound.state,
      replay.context,
    );
    expect(replayObservation?.transitionKind).toBe("apply_block");
    const replayed = evaluateWatcherUserEventIndexer(
      policy,
      rewound.state,
      replayObservation,
      replay.context,
    );
    expect(replayed).toMatchObject({
      action: "accept",
      reasonCodes: ["block_authenticated"],
      state: { snapshot: { activeEvents: [] } },
    });
  });

  it("applies an exact W13 rewind, survives serialized restart, deactivates the orphan, and permits re-inclusion", () => {
    const olderIdenticalBlock = blockBundle([], null, 89, 1);
    const olderIdenticalState = accepted(null, olderIdenticalBlock);
    const bootstrapBlock = blockBundle([], olderIdenticalBlock.store, 90, 1);
    const bootstrapState = accepted(olderIdenticalState, bootstrapBlock);
    const fixture = makeEventFixture("deposit", "b3", 0, 1_000n);
    const created = blockBundle([fixture], bootstrapBlock.store, 91, 1);
    const active = accepted(bootstrapState, created);
    expect(active.snapshot.activeEvents).toHaveLength(1);

    const rollback = rollbackBundle(created);
    expect(rollback.applied.removedRecords.protocolUtxoOutRefs).toContain(
      active.snapshot.activeEvents[0]!.outRef,
    );
    const targetDigest = bootstrapState.activeEntryDigests.at(-1)!;
    expect(
      deriveWatcherUserEventObservation(
        policy,
        active,
        rollback.context,
        olderIdenticalState.activeEntryDigests.at(-1)!,
      ),
    ).toBeNull();
    const observation = deriveWatcherUserEventObservation(
      policy,
      active,
      rollback.context,
      targetDigest,
    );
    expect(observation).not.toBeNull();
    const rewound = evaluateWatcherUserEventIndexer(
      policy,
      active,
      observation,
      rollback.context,
    );
    expect(rewound).toMatchObject({
      action: "accept",
      reasonCodes: ["rollback_authenticated"],
      state: {
        snapshot: { activeEvents: [] },
      },
    });
    const restarted = parseWatcherUserEventIndexerState(
      JSON.parse(JSON.stringify(rewound.state)),
      policy,
    );
    expect(restarted).toEqual(rewound.state);
    expect(restarted!.activeEntryDigests).not.toContain(
      active.activeEntryDigests.at(-1),
    );

    const reinclusion = blockBundle(
      [fixture],
      rollback.applied.nextStore,
      91,
      1,
    );
    const reincluded = accepted(restarted, reinclusion);
    expect(reincluded.snapshot.activeEvents).toHaveLength(1);
    expect(reincluded.history.length).toBe(restarted!.history.length + 1);
  });

  it.each(["external_providers"] as const)(
    "consumes an exact %s W13 post-finality recovery, prunes the orphan lineage, and resumes idempotently",
    () => {
      const commonBundle = blockBundle([], null, 1_100, 0, "mint", undefined);
      const commonState = accepted(null, commonBundle);
      const fixture = makeEventFixture("deposit", "d9", 0, 1_000n);
      const orphanBundle = blockBundle(
        [fixture],
        commonBundle.store,
        1_101,
        1,
        "mint",
        undefined,
      );
      const orphanState = accepted(commonState, orphanBundle);
      const recovery = postFinalityUserEventRecoveryBundle(
        commonBundle,
        orphanBundle,
      );
      const orphan = orphanState.snapshot.activeEvents[0]!;
      expect(recovery.recovery.removedRecords.protocolUtxoOutRefs).toContain(
        orphan.outRef,
      );
      const targetEntryDigest = commonState.activeEntryDigests.at(-1)!;
      const observation = deriveWatcherUserEventObservation(
        policy,
        orphanState,
        recovery.context,
        targetEntryDigest,
      );
      expect(observation).not.toBeNull();
      const applied = evaluateWatcherUserEventIndexer(
        policy,
        orphanState,
        observation,
        recovery.context,
      );
      expect(applied).toMatchObject({
        action: "accept",
        protocolDecision: "indexed",
        reasonCodes: ["rollback_authenticated"],
        state: {
          snapshot: {
            activeEvents: [],
            terminalEvents: [],
            quarantined: false,
          },
        },
      });
      expect(applied.state?.history).toHaveLength(
        orphanState.history.length + 1,
      );
      expect(applied.state?.history).toContainEqual(orphanState.history.at(-1));
      expect(applied.state?.activeEntryDigests).not.toContain(
        orphanState.activeEntryDigests.at(-1),
      );
      const persistedStore = parseWatcherDurableStore(
        applied.state?.history.at(-1)?.publicContext.durableStore,
      );
      expect(persistedStore.protocolUtxos).not.toContainEqual(
        expect.objectContaining({ outRef: orphan.outRef }),
      );
      for (const observationId of recovery.replacementObservationIds) {
        expect(persistedStore.l1Observations).toContainEqual(
          expect.objectContaining({ observationId }),
        );
      }

      const serializedState = JSON.parse(JSON.stringify(applied.state));
      const serializedContext =
        serializedState.history.at(-1)?.publicContext ?? null;
      expect(
        deriveWatcherUserEventObservation(
          policy,
          orphanState,
          serializedContext,
          targetEntryDigest,
        ),
      ).toEqual(observation);
      expect(
        evaluateWatcherUserEventIndexer(
          policy,
          orphanState,
          observation,
          serializedContext,
        ).state,
      ).toEqual(applied.state);
      const restarted = parseWatcherUserEventIndexerState(
        serializedState,
        policy,
      );
      expect(restarted).toEqual(applied.state);
      expect(
        evaluateWatcherUserEventIndexer(
          policy,
          restarted,
          observation,
          recovery.context,
        ),
      ).toMatchObject({
        action: "duplicate",
        protocolDecision: "indexed",
        state: applied.state,
      });

      const resumedBundle = blockBundle(
        [],
        recovery.recovery.nextStore,
        1_102,
        1,
        "mint",
        undefined,
      );
      const resumed = accepted(restarted, resumedBundle);
      expect(resumed.snapshot.quarantined).toBe(false);
      expect(resumed.history).toHaveLength(restarted!.history.length + 1);
    },
    30_000,
  );

  it("derives a post-finality target internally and records a no-owned-change recovery", () => {
    const commonBundle = blockBundle([], null, 1_150, 0);
    const commonState = accepted(null, commonBundle);
    const orphanBundle = blockBundle(
      [makeEventFixture("deposit", "df", 0, 1_000n)],
      commonBundle.store,
      1_151,
      1,
    );
    const recovery = postFinalityUserEventRecoveryBundle(
      commonBundle,
      orphanBundle,
    );
    const derivedWithoutCallerTarget = deriveWatcherUserEventObservation(
      policy,
      commonState,
      recovery.context,
    );
    expect(derivedWithoutCallerTarget).not.toBeNull();
    expect(
      deriveWatcherUserEventObservation(
        policy,
        commonState,
        recovery.context,
        h32("ff"),
      ),
    ).toEqual(derivedWithoutCallerTarget);
    const applied = evaluateWatcherUserEventIndexer(
      policy,
      commonState,
      derivedWithoutCallerTarget,
      recovery.context,
    );
    expect(applied).toMatchObject({
      action: "accept",
      reasonCodes: ["rollback_authenticated"],
      state: {
        snapshot: commonState.snapshot,
        activeEntryDigests: [
          ...commonState.activeEntryDigests,
          expect.any(String),
        ],
      },
    });
    expect(applied.state?.history).toHaveLength(commonState.history.length + 1);
    expect(
      parseWatcherUserEventIndexerState(structuredClone(applied.state), policy),
    ).toEqual(applied.state);
  }, 30_000);

  it("rejects forged, mismatched, wrong-target, mode-invalid, and duplicate-only post-finality recovery authorities", () => {
    const commonBundle = blockBundle([], null, 1_200, 0);
    const commonState = accepted(null, commonBundle);
    const orphanBundle = blockBundle(
      [makeEventFixture("deposit", "da", 0, 1_000n)],
      commonBundle.store,
      1_201,
      1,
    );
    const orphanState = accepted(commonState, orphanBundle);
    const recovery = postFinalityUserEventRecoveryBundle(
      commonBundle,
      orphanBundle,
    );
    const targetEntryDigest = commonState.activeEntryDigests.at(-1)!;
    const derive = (context: WatcherUserEventPublicContext) =>
      deriveWatcherUserEventObservation(
        policy,
        orphanState,
        context,
        targetEntryDigest,
      );

    const forgedContext = structuredClone(
      recovery.context,
    ) as WatcherUserEventPublicContext;
    const forgedResult = forgedContext.rollbackAuthority!
      .result as MutableRecord;
    forgedResult.nextStoreDigest = h32("ff");
    const { resultDigest: _resultDigest, ...forgedCanonical } = forgedResult;
    forgedResult.resultDigest = sha256Canonical(forgedCanonical);
    expect(derive(forgedContext)).toBeNull();

    const mismatchedContext = structuredClone(
      recovery.context,
    ) as WatcherUserEventPublicContext;
    (
      mismatchedContext.rollbackAuthority!.context as unknown as MutableRecord
    ).replacementCanonicalPath = recovery.recoveryInput.previousCanonicalPath;
    expect(derive(mismatchedContext)).toBeNull();

    const modeInvalidContext = structuredClone(
      recovery.context,
    ) as WatcherUserEventPublicContext;
    (
      modeInvalidContext.rollbackAuthority!.context as unknown as MutableRecord
    ).policy = {
      ...finalityPolicy,
      sourceMode: "local_node",
    };
    expect(derive(modeInvalidContext)).toBeNull();

    expect(
      deriveWatcherUserEventObservation(
        policy,
        orphanState,
        recovery.context,
        orphanState.activeEntryDigests.at(-1)!,
      ),
    ).toEqual(derive(recovery.context));

    const duplicateRecovery = evaluateWatcherPostFinalityRecovery({
      ...recovery.recoveryInput,
      currentStore: recovery.recovery.nextStore,
      previousRecoveryState: recovery.recovery.recoveryState,
    });
    expect(duplicateRecovery.action).toBe("duplicate_recovery");
    const duplicateOnlyContext = {
      ...recovery.context,
      sourceDurableStore: recovery.recovery.nextStore,
      rollbackAuthority: {
        result: duplicateRecovery,
        context: {
          ...recovery.recoveryInput,
          currentStore: recovery.recovery.nextStore,
          previousRecoveryState: recovery.recovery.recoveryState,
        },
      },
    };
    expect(derive(duplicateOnlyContext)).toBeNull();
  }, 30_000);

  it("retains append-only foreign durable records through an authenticated rewind", () => {
    const olderBlock = blockBundle([], null, 92, 1);
    const olderState = accepted(null, olderBlock);
    const targetBlock = blockBundle([], olderBlock.store, 93, 1);
    const targetState = accepted(olderState, targetBlock);
    const fixture = makeEventFixture("deposit", "ba", 0, 1_000n);
    const created = blockBundle([fixture], targetBlock.store, 94, 1);
    const active = accepted(targetState, created);
    const sentinelDaInput = {
      inputId: h32("d4"),
      kind: "da_payload" as const,
      payload: makeWatcherDurablePayload("01"),
    };
    const sentinelUnrelatedUtxo = {
      outRef: `${h32("d5")}#0`,
      role: "payout" as const,
      chainPointId: BOOTSTRAP_CHAIN_POINT_ID,
      output: makeWatcherDurablePayload("02"),
    };
    const rollback = rollbackBundle(created, [], (sourceStore) =>
      rebuildRollbackSourceStore(sourceStore, {
        protocolUtxos: [...sourceStore.protocolUtxos, sentinelUnrelatedUtxo],
        daProofInputs: [...sourceStore.daProofInputs, sentinelDaInput],
      }),
    );
    expect(rollback.applied.nextStore?.daProofInputs).toContainEqual(
      sentinelDaInput,
    );
    expect(rollback.applied.nextStore?.protocolUtxos).toContainEqual(
      sentinelUnrelatedUtxo,
    );

    const observation = deriveWatcherUserEventObservation(
      policy,
      active,
      rollback.context,
      targetState.activeEntryDigests.at(-1)!,
    );
    expect(observation).not.toBeNull();
    const rewound = evaluateWatcherUserEventIndexer(
      policy,
      active,
      observation,
      rollback.context,
    );
    expect(rewound.action).toBe("accept");
    const persistedStore = parseWatcherDurableStore(
      rewound.state?.history.at(-1)?.publicContext.durableStore,
    );
    expect(persistedStore.daProofInputs).toContainEqual(sentinelDaInput);
    expect(persistedStore.protocolUtxos).toContainEqual(sentinelUnrelatedUtxo);
  });

  it("rejects rollback sources that delete, mutate, or collide with anchored durable records", () => {
    const targetBlock = blockBundle([], null, 95, 1);
    const targetState = accepted(null, targetBlock);
    const fixture = makeEventFixture("deposit", "bb", 0, 1_000n);
    const created = blockBundle([fixture], targetBlock.store, 96, 1);
    const active = accepted(targetState, created);
    const targetDigest = targetState.activeEntryDigests.at(-1)!;
    const deriveHostile = (
      transform: (sourceStore: WatcherDurableStore) => WatcherDurableStore,
    ) => {
      const rollback = rollbackBundle(created, [], transform);
      return deriveWatcherUserEventObservation(
        policy,
        active,
        rollback.context,
        targetDigest,
      );
    };

    expect(
      deriveHostile((sourceStore) =>
        rebuildRollbackSourceStore(sourceStore, {
          protocolUtxos: sourceStore.protocolUtxos.filter(
            ({ outRef }) => outRef !== HUB_OUT_REF,
          ),
        }),
      ),
    ).toBeNull();
    expect(
      deriveHostile((sourceStore) =>
        rebuildRollbackSourceStore(sourceStore, {
          protocolUtxos: sourceStore.protocolUtxos.map((utxo) =>
            utxo.outRef === SETTLEMENT_OUT_REF
              ? {
                  ...utxo,
                  output: makeWatcherDurablePayload("03"),
                }
              : utxo,
          ),
        }),
      ),
    ).toBeNull();
    expect(
      deriveHostile((sourceStore) =>
        rebuildRollbackSourceStore(sourceStore, {
          protocolUtxos: sourceStore.protocolUtxos.map((utxo) =>
            utxo.role === "deposit"
              ? { ...utxo, role: "payout" as const }
              : utxo,
          ),
        }),
      ),
    ).toBeNull();
    expect(
      deriveHostile((sourceStore) =>
        rebuildRollbackSourceStore(
          sourceStore,
          {
            daProofInputs: [
              ...sourceStore.daProofInputs,
              {
                inputId: h32("d6"),
                kind: "da_payload" as const,
                payload: makeWatcherDurablePayload("04"),
              },
            ],
          },
          created.store.revision,
        ),
      ),
    ).toBeNull();

    const spentCreated = blockBundle([
      makeEventFixture("deposit", "bc", 0, 1_000n),
    ]);
    const spentActive = accepted(null, spentCreated);
    const terminalBundle = depositSpendBundle(spentActive, spentCreated.store);
    const terminal = accepted(spentActive, terminalBundle);
    const restoredEventUtxos = spentCreated.store.protocolUtxos.filter(
      ({ role }) => role === "deposit",
    );
    const removedSpentEvent = rollbackBundle(
      terminalBundle,
      restoredEventUtxos,
      (sourceStore) =>
        rebuildRollbackSourceStore(sourceStore, {
          spentProtocolUtxos: sourceStore.spentProtocolUtxos.filter(
            ({ role }) => role !== "deposit",
          ),
        }),
    );
    expect(
      deriveWatcherUserEventObservation(
        policy,
        terminal,
        removedSpentEvent.context,
        spentActive.activeEntryDigests.at(-1)!,
      ),
    ).toBeNull();
  });

  it("rejects adjacent inclusion time, malformed canonical datum, wrong network/address, policy, witness, and duplicate evidence", () => {
    const wrongTime = makeEventFixture("deposit", "c1", 0, 1_000n, 1n);
    expect(
      deriveWatcherUserEventObservation(
        policy,
        null,
        blockBundle([wrongTime]).context,
      ),
    ).toBeNull();
    const wrongAddress = makeEventFixture(
      "deposit",
      "c3",
      0,
      1_000n,
      0n,
      scriptAddress(h28("99")),
    );
    expect(
      deriveWatcherUserEventObservation(
        policy,
        null,
        blockBundle([wrongAddress]).context,
      ),
    ).toBeNull();
    const wrongWitness = makeEventFixture(
      "deposit",
      "c4",
      0,
      1_000n,
      0n,
      undefined,
      h28("9a"),
    );
    expect(
      deriveWatcherUserEventObservation(
        policy,
        null,
        blockBundle([wrongWitness]).context,
      ),
    ).toBeNull();

    const valid = makeEventFixture("deposit", "c2", 0, 1_000n);
    expect(
      deriveWatcherUserEventObservation(
        policy,
        null,
        blockBundle([valid], null, 100, 1, "spend").context,
      ),
    ).toBeNull();
    const wrongPolicy = {
      ...valid,
      fields: {
        ...valid.fields,
        policyId: h28("9b"),
      },
    };
    expect(
      deriveWatcherUserEventObservation(
        policy,
        null,
        blockBundle([wrongPolicy]).context,
      ),
    ).toBeNull();
    const validBundle = blockBundle([valid]);
    const state = accepted(null, validBundle);
    const observation = state.history[0]!.observation;
    expect(
      evaluateWatcherUserEventIndexer(
        policy,
        state,
        observation,
        validBundle.context,
      ),
    ).toMatchObject({
      action: "duplicate",
      reasonCodes: ["duplicate_observation"],
    });
    expect(
      deriveWatcherUserEventObservation(
        policy,
        state,
        blockBundle([valid], validBundle.store, 101, 1).context,
      ),
    ).toBeNull();

    const wrongNetworkPolicy = {
      ...policy,
      network: "Mainnet",
    };
    expect(
      deriveWatcherUserEventObservation(
        wrongNetworkPolicy,
        null,
        validBundle.context,
      ),
    ).toBeNull();

    const malformed = structuredClone(validBundle.context) as Record<
      string,
      any
    >;
    malformed.l1Observation.transactions[0].utxos[0].datum.bytes.bytesHex +=
      "00";
    malformed.l1Observation.transactions[0].utxos[0].datum.bytes.sha256 =
      sha256(
        Buffer.from(
          malformed.l1Observation.transactions[0].utxos[0].datum.bytes.bytesHex,
          "hex",
        ),
      );
    expect(
      deriveWatcherUserEventObservation(policy, null, malformed),
    ).toBeNull();

    const forgedFinality = structuredClone(validBundle.context) as Record<
      string,
      any
    >;
    forgedFinality.finalityAuthority.result.resultDigest = h32("fd");
    expect(
      deriveWatcherUserEventObservation(policy, null, forgedFinality),
    ).toBeNull();

    const providerDisagreement = structuredClone(validBundle.context) as Record<
      string,
      any
    >;
    const disagreementRaw = structuredClone(providerDisagreement.l1Observation);
    disagreementRaw.providerId = providerB.providerId;
    disagreementRaw.transactions = [];
    providerDisagreement.finalityAuthority.consistency =
      evaluateWatcherMultiProviderConsistency(externalSource, [
        normalizeWatcherL1Block(provider, providerDisagreement.l1Observation),
        normalizeWatcherL1Block(providerB, disagreementRaw),
      ]);
    providerDisagreement.finalityAuthority.result = evaluateWatcherFinality(
      finalityPolicy,
      providerDisagreement.finalityAuthority.previousState,
      providerDisagreement.finalityAuthority.consistency,
    );
    expect(
      deriveWatcherUserEventObservation(policy, null, providerDisagreement),
    ).toBeNull();
  });

  it("rejects adjacent constructor tags, arities, and global-list index substitutions across every event redeemer", () => {
    const fixture = makeEventFixture("deposit", "c5", 0, 1_000n);
    for (const redeemerIndex of [0, 1]) {
      for (const mutate of [adjacentConstructor, truncatedConstructor]) {
        const hostile = blockBundle(
          [fixture],
          null,
          110,
          1,
          "mint",
          (redeemers) => {
            const entry = redeemers[redeemerIndex]!;
            entry.bytes = makeWatcherL1PublicBytes(
              mutate(entry.bytes.bytesHex),
            );
          },
        );
        expect(
          deriveWatcherUserEventObservation(policy, null, hostile.context),
        ).toBeNull();
      }
    }
    const wrongGlobalIndex = blockBundle(
      [fixture],
      null,
      111,
      1,
      "mint",
      (redeemers) => {
        redeemers[0]!.bytes = makeWatcherL1PublicBytes(
          encodeData(
            {
              AuthenticateEvent: {
                nonce_input_index: 0n,
                event_output_index: 0n,
                hub_ref_input_index: 0n,
                witness_registration_redeemer_index: 0n,
              },
            },
            UserEventMintRedeemer,
          ),
        );
      },
    );
    expect(
      deriveWatcherUserEventObservation(policy, null, wrongGlobalIndex.context),
    ).toBeNull();

    const created = blockBundle([fixture], null, 100, 1);
    const active = accepted(null, created);
    for (const redeemerIndex of [0, 1, 2]) {
      for (const mutate of [adjacentConstructor, truncatedConstructor]) {
        const hostile = depositSpendBundle(
          active,
          created.store,
          false,
          (redeemers) => {
            const entry = redeemers[redeemerIndex]!;
            entry.bytes = makeWatcherL1PublicBytes(
              mutate(entry.bytes.bytesHex),
            );
          },
        );
        expect(
          deriveWatcherUserEventObservation(policy, active, hostile.context),
        ).toBeNull();
      }
    }
    const malformedMembership = depositSpendBundle(
      active,
      created.store,
      false,
      (redeemers) => {
        redeemers[3]!.bytes = makeWatcherL1PublicBytes(
          truncatedList(redeemers[3]!.bytes.bytesHex),
        );
      },
    );
    expect(
      deriveWatcherUserEventObservation(
        policy,
        active,
        malformedMembership.context,
      ),
    ).toBeNull();

    for (const [kind, nonce] of [
      ["withdrawal", "c7"],
      ["forced_order", "c8"],
    ] as const) {
      const nonDepositFixture = makeEventFixture(kind, nonce, 0, 1_000n);
      const nonDepositCreated = blockBundle([nonDepositFixture], null, 100, 1);
      const nonDepositActive = accepted(null, nonDepositCreated);
      const redeemerIndices = kind === "withdrawal" ? [0, 2] : [0];
      for (const redeemerIndex of redeemerIndices) {
        for (const mutate of [adjacentConstructor, truncatedConstructor]) {
          const hostile = nonDepositSpendBundle(
            nonDepositActive,
            nonDepositCreated.store,
            (redeemers) => {
              const entry = redeemers[redeemerIndex]!;
              entry.bytes = makeWatcherL1PublicBytes(
                mutate(entry.bytes.bytesHex),
              );
            },
          );
          expect(
            deriveWatcherUserEventObservation(
              policy,
              nonDepositActive,
              hostile.context,
            ),
          ).toBeNull();
        }
      }
    }
  });

  it("rejects unsigned deployment substitutions and rehashed store subset, revision, and chain-point attacks", () => {
    const fixture = makeEventFixture("deposit", "c6", 0, 1_000n);
    const initial = blockBundle([fixture], null, 100, 1);
    const forgedDeployment = structuredClone(initial.context) as MutableRecord;
    forgedDeployment.deploymentAuthority.policy.appliedScriptHashes.depositMint =
      h28("ee");
    expect(
      deriveWatcherUserEventObservation(policy, null, forgedDeployment),
    ).toBeNull();

    const state = accepted(null, initial);
    const successor = blockBundle([], initial.store, 121, 1);
    const source = successor.context.sourceDurableStore as ReturnType<
      typeof makeWatcherDurableStore
    >;
    const next = successor.context.durableStore as ReturnType<
      typeof makeWatcherDurableStore
    >;
    const subset = structuredClone(successor.context) as MutableRecord;
    subset.durableStore = makeWatcherDurableStore({
      deploymentMarker: next.deploymentMarker,
      revision: next.revision,
      records: {
        ...next,
        l1Observations: next.l1Observations.slice(-1),
      },
    });
    expect(deriveWatcherUserEventObservation(policy, state, subset)).toBeNull();

    const jumpedRevision = structuredClone(successor.context) as MutableRecord;
    jumpedRevision.durableStore = makeWatcherDurableStore({
      deploymentMarker: next.deploymentMarker,
      revision: (BigInt(source.revision) + 2n).toString(),
      records: next,
    });
    expect(
      deriveWatcherUserEventObservation(policy, state, jumpedRevision),
    ).toBeNull();

    const reassignedPoint = structuredClone(successor.context) as MutableRecord;
    reassignedPoint.durableStore = makeWatcherDurableStore({
      deploymentMarker: next.deploymentMarker,
      revision: next.revision,
      records: {
        ...next,
        protocolUtxos: next.protocolUtxos.map((utxo) =>
          utxo.role === "deposit"
            ? { ...utxo, chainPointId: BOOTSTRAP_CHAIN_POINT_ID }
            : utxo,
        ),
      },
    });
    expect(
      deriveWatcherUserEventObservation(policy, state, reassignedPoint),
    ).toBeNull();

    const terminal = depositSpendBundle(state, initial.store);
    const terminalStore = terminal.context.durableStore as ReturnType<
      typeof makeWatcherDurableStore
    >;
    const omittedArchive = structuredClone(terminal.context) as MutableRecord;
    omittedArchive.durableStore = makeWatcherDurableStore({
      deploymentMarker: terminalStore.deploymentMarker,
      revision: terminalStore.revision,
      records: {
        ...terminalStore,
        spentProtocolUtxos: [],
      },
    });
    expect(
      deriveWatcherUserEventObservation(policy, state, omittedArchive),
    ).toBeNull();

    const substitutedArchive = structuredClone(
      terminal.context,
    ) as MutableRecord;
    substitutedArchive.durableStore = makeWatcherDurableStore({
      deploymentMarker: terminalStore.deploymentMarker,
      revision: terminalStore.revision,
      records: {
        ...terminalStore,
        spentProtocolUtxos: terminalStore.spentProtocolUtxos.map((entry) => ({
          ...entry,
          spentAtChainPointId: entry.chainPointId,
        })),
      },
    });
    expect(
      deriveWatcherUserEventObservation(policy, state, substitutedArchive),
    ).toBeNull();
  });

  it("rejects forged state/history/public results even when attackers recompute outer digests", () => {
    const fixture = makeEventFixture("deposit", "d1", 0, 1_000n);
    const bundle = blockBundle([fixture]);
    const state = accepted(null, bundle);
    const forged = structuredClone(state) as Record<string, any>;
    forged.snapshot.activeEvents[0].eventCborHex = "d87980";
    forged.snapshot.snapshotDigest = h32("ef");
    forged.stateDigest = h32("fe");
    expect(parseWatcherUserEventIndexerState(forged, policy)).toBeNull();

    const successor = blockBundle([], bundle.store, 101, 1);
    const observation = deriveWatcherUserEventObservation(
      policy,
      state,
      successor.context,
    );
    expect(observation).not.toBeNull();
    const result = evaluateWatcherUserEventIndexer(
      policy,
      state,
      observation,
      blockBundle([], bundle.store, 104, 1).context,
    );
    expect(result.action).toBe("reject");
  });
});
