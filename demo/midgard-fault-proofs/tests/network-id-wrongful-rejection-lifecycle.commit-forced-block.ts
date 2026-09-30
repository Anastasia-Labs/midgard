import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  encodeMidgardFieldPreimage,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";

import {
  type NetworkIdForcedScanStep,
  type PreparedNetworkIdWrongfulRejection,
} from "../src/network-id/index.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { makeNetworkIdEmulatorHarness } from "./support/network-id-emulator.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";

export const network = "Custom" as const;

const EXPECTED_NETWORK_ID = 0n;

export const NETWORK_ID_MISMATCH = {
  ForcedTxInvalid: { reason: "NetworkIdMismatch" },
};

export type Harness = Awaited<ReturnType<typeof makeNetworkIdEmulatorHarness>>;

type NetworkIdForcedScanAdvance = Extract<
  NetworkIdForcedScanStep,
  { readonly kind: "advance" }
>;

/** A planned `Advance` with one field deliberately changed. */
export const mutatedAdvance = (
  step: NetworkIdForcedScanStep,
  patch: Partial<NetworkIdForcedScanAdvance>,
): NetworkIdForcedScanStep => {
  if (step.kind !== "advance") {
    throw new Error("scan mutation expects a planned Advance batch");
  }
  return { ...step, ...patch };
};

/** One forced native transaction: its outputs live at the given network ids. */
export const forcedTransactionAt = ({
  outputNetworkIds,
  bodyNetworkId = 0n,
  inputByte = "77",
}: {
  readonly outputNetworkIds: readonly number[];
  readonly bodyNetworkId?: bigint;
  readonly inputByte?: string;
}) => {
  const outputs = outputNetworkIds.map((networkId, index) =>
    encodeMidgardTxOutput({
      // Enterprise key address: header nibble `6`, low nibble = network id.
      address: Buffer.concat([
        Buffer.from([0x60 | networkId]),
        Buffer.alloc(28, 0x40 + (index % 64)),
      ]),
      value: { lovelace: 2_000_000n + BigInt(index), assets: new Map() },
    }),
  );
  const invalid = materializeMidgardForcedTxFromCanonical(
    makeNativeTx({
      spendInputCbors: [
        encodeMidgardSpendInputItem({
          txId: Buffer.from(inputByte.repeat(32), "hex"),
          outputIndex: 0,
        }),
      ],
      outputCbors: outputs,
      fee: 0n,
      networkId: bodyNetworkId,
    }),
  );
  const proofSource = deriveMidgardForcedTxProofSource(invalid);
  return {
    transactionId: computeMidgardNativeTxId(invalid).toString("hex"),
    outputs,
    bodyNetworkId,
    outputNetworkIds,
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
};

type ForcedTransaction = ReturnType<typeof forcedTransactionAt>;

type ForcedLeaf = {
  /** Output index of the leaf's source key under the fixture's forced event. */
  readonly outputIndex: bigint;
  readonly value: {
    readonly tx_id: string;
    readonly submitted_source: ForcedTransaction["source"];
    readonly verdict: unknown;
  };
};

/**
 * Commits the given forced leaves to one counted root, publishes the disputed
 * block, and returns a membership proof per leaf. Leaves are committed exactly
 * as given so a dishonest operator's leaf (wrong reason, wrong tx id) is
 * authenticated by the root the way the chain will see it.
 */
export const commitForcedBlock = async (
  harness: Harness,
  leaves: readonly ForcedLeaf[],
) => {
  const credential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("funder key absent");
  const base = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
  });
  const keyed = leaves.map((leaf) => ({
    ...leaf,
    key: {
      ...base.eventKey.ForcedTransactionEventKey.tx_order_id,
      outputIndex: leaf.outputIndex,
    },
  }));
  const encoded = keyed.map((leaf) => ({
    key: Buffer.from(Data.to(leaf.key, OutputReference), "hex"),
    value: Buffer.from(
      Data.to(leaf.value as never, ForcedInclusionTxV1Schema as never),
      "hex",
    ),
  }));
  const root = await buildCountedRoot(
    ROOT_DOMAINS.forcedTransactionsV1,
    encoded,
  );
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const { key, value } of encoded) await trie.insert(key, value);
  const memberships: PreparedNetworkIdWrongfulRejection["forcedSource"]["membership"][] =
    await Promise.all(
      keyed.map(async (leaf, index) => ({
        domain: root.domain,
        root: root.root,
        phas_root: root.phasRoot,
        count: root.count,
        key: leaf.key,
        value: leaf.value,
        proof: Data.from(
          (await trie.prove(encoded[index]!.key)).toCBOR().toString("hex"),
          Proof,
        ),
      })) as never,
    );
  const header = {
    ...base.header,
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: BigInt(leaves.length),
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  return { base, header, setup, root, memberships };
};

/** The prepared artifact the planner would build, from retained payload only. */
export const preparedFor = ({
  transaction,
  headerHash,
  header,
  membership,
  claimedOutputNetworkIds = transaction.outputNetworkIds.map(BigInt),
  claimedBodyNetworkId = transaction.bodyNetworkId,
}: {
  readonly transaction: ForcedTransaction;
  readonly headerHash: string;
  readonly header: Awaited<ReturnType<typeof commitForcedBlock>>["header"];
  readonly membership: Awaited<
    ReturnType<typeof commitForcedBlock>
  >["memberships"][number];
  readonly claimedOutputNetworkIds?: readonly bigint[];
  readonly claimedBodyNetworkId?: bigint;
}): PreparedNetworkIdWrongfulRejection => {
  const subject = forcedVerdictSubject({
    transactionId: transaction.transactionId,
    sourceKey: membership.key,
    rejectionReason: "NetworkIdMismatch",
  });
  const outputsItemCbors = transaction.outputs.map((output) =>
    output.toString("hex"),
  );
  return {
    headerHash,
    expectedNetworkId: EXPECTED_NETWORK_ID,
    badTxId: transaction.transactionId,
    nativeTxCompactCbor: transaction.source.compact_cbor,
    outputsItemCbors,
    faultClaim: { kind: "forced-network-mismatch" },
    fault: "ForcedNetworkIdMismatch",
    subject,
    forcedSource: { header, membership, direction: 1n },
    evidence: {
      subject,
      expectedNetworkId: EXPECTED_NETWORK_ID,
      committedNetworkId: claimedBodyNetworkId,
      outputNetworkIds: claimedOutputNetworkIds,
      outputsItemCbors,
      outputsPreimageCbor: encodeMidgardFieldPreimage(
        transaction.outputs,
      ).toString("hex"),
    },
  };
};
