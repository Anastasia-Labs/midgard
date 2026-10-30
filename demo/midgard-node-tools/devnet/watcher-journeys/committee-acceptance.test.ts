import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as canonical,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  generateEmulatorAccount,
  type UTxO,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import {
  DaPayloadValidationError,
  verifyDaPayloadAgainstHeader,
} from "da-committee-node/da/payload";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { JOURNEY_FIXTURE_CANDIDATES } from "./catalogue.js";
import type { JourneyBlock, JourneyCategory } from "./fixture.js";
import {
  buildJourneyHistoryTransaction,
  JOURNEY_HISTORY_TRANSACTION_CATEGORIES,
} from "./history-cases.js";
import {
  buildJourneyFabricatedDeposit,
  buildJourneyWithdrawalEvent,
  buildJourneyWithdrawnTransaction,
  journeyWithdrawalBody,
  type StagedHistoryEvent,
} from "./history-events.js";
import {
  checkedFixtureBlock,
  depositEventsRetainedBlock,
} from "./journey-fixture-ledger.test-support.js";
import { journeyLedgerPairs } from "./journey-ledger.js";
import {
  buildJourneyScriptForcedFault,
  JOURNEY_SCRIPT_FORCED_CATEGORIES,
} from "./script-cases.js";
import { buildJourneyValidationTraceFault } from "./script-validation-trace-case.js";
import {
  buildJourneyTransactionControl,
  buildJourneyTransactionFault,
  JOURNEY_TRANSACTION_CATEGORIES,
} from "./transaction-cases.js";
import {
  buildJourneyForcedTransaction,
  JOURNEY_FORCED_TRANSACTION_CATEGORIES,
} from "./transaction-forced-cases.js";
import {
  buildJourneyTransactionSourceFault,
  JOURNEY_TRANSACTION_SOURCE_CATEGORIES,
} from "./transaction-source-cases.js";

/**
 * What an honest DA committee does with each family's fault payload: sign it
 * ("accepted") or refuse it with the committee's own refusal code. Only an
 * accepted family can be drilled on a stack whose committee is honest; a
 * refused one is a refusal check there, never a conviction.
 */
type CommitteeVerdict = "accepted" | DaPayloadValidationError["code"];

const COMMITTEE_ACCEPTANCE = {
  transitionTrace: "accepted",
  invalidRange: "accepted",
  zeroInput: "accepted",
  daHashPreimage: "malformed_transaction",
  invalidSignature: "accepted",
  missingSignature: "accepted",
  canonicalDecodability: "malformed_transaction",
  committedFieldShape: "accepted",
  minFee: "accepted",
  l2TxMistag: "unsupported_feature",
  valueNotPreserved: "accepted",
  inputSetUniqueness: "accepted",
  mintAuthorization: "accepted",
  networkId: "accepted",
  minAda: "accepted",
  fieldPreimageLengthMismatch: "accepted",
  fieldItemWidthIllegal: "accepted",
  transactionOutputNonCanonical: "accepted",
  resolvedOutputNonCanonical: "accepted",
  mintDeclaredAssetLimit: "accepted",
  mintItemNonCanonical: "accepted",
  spendInputSignerMissing: "accepted",
  protectedOutputSignerMissing: "accepted",
  observersForbiddenOnUntaggedNetwork: "accepted",
  observerOrderInvalid: "accepted",
  nativeScriptDecoding: "accepted",
  nativeScriptInvalid: "accepted",
  witnessScriptDecoding: "accepted",
  scriptIntegrityHashMissing: "accepted",
  outputReferenceScriptDecoding: "accepted",
  redeemerCanonicity: "accepted",
  validationTraceDispute: "accepted",
  missingRedeemer: "accepted",
  missingScriptSource: "accepted",
  unusedRedeemer: "accepted",
  receivePurposeLanguage: "accepted",
  scriptIntegrityHashMismatch: "accepted",
  executionSourceScriptDecoding: "accepted",
  executionNativeScriptInvalid: "accepted",
  distinctAssetAccumulationLimit: "accepted",
  unusedScriptWitness: "accepted",
  doubleSpend: "accepted",
  nonExistentInput: "accepted",
  nonExistentInputNoIndex: "accepted",
  noReferenceInput: "accepted",
  referenceInputNoIdx: "accepted",
  fabricatedDeposit: "accepted",
  fabricatedWithdrawal: "accepted",
  withdrawnReferenceInput: "accepted",
  withdrawalMistag: "accepted",
  doubleWithdraw: "accepted",
  withdrawnInput: "accepted",
} as const satisfies Record<JourneyCategory, CommitteeVerdict>;

/** The committee's whole content check on a stored payload for an L1 header. */
const committeeVerdict = async (
  block: JourneyBlock,
): Promise<CommitteeVerdict> => {
  try {
    await verifyDaPayloadAgainstHeader(
      block.payloadEnvelopeCbor,
      block.headerHash,
      block.header,
      {
        payloadSchemaVersion: 1,
        stateQueueOutRef: `${"5c".repeat(32)}#0`,
      },
    );
    return "accepted";
  } catch (cause) {
    if (cause instanceof DaPayloadValidationError) return cause.code;
    throw cause;
  }
};

/**
 * One ledger owner holding two outputs: event families withdraw one and keep
 * the other, every other family spends from them. Times sit far from zero so
 * that every event inclusion time falls strictly inside the fault interval.
 */
const openPredecessor = async () => {
  const account = generateEmulatorAccount({ lovelace: 100_000_000n });
  const wallet = walletFromSeed(account.seedPhrase, { network: "Custom" });
  const ownerKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
  const owner = ownerKey.to_public().hash().to_raw_bytes();
  const ownedOutput = (lovelace: bigint) =>
    encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.from(owner)]),
      value: { lovelace, assets: new Map() },
    });
  const predecessor = await checkedFixtureBlock(
    await buildCanonicalBlockFixture({
      transactions: [],
      utxos: [
        { key: outRefCbor(0x81, 0n), value: ownedOutput(100_000_000n) },
        { key: outRefCbor(0x81, 1n), value: ownedOutput(50_000_000n) },
      ],
    }),
  );
  return {
    predecessor,
    ownerKey,
    owner: Buffer.from(owner).toString("hex"),
    input: {
      predecessor,
      ledgerOwnerSeedPhrase: account.seedPhrase,
      operatorVkey: "82".repeat(28),
      endTime: predecessor.header.endTime + 40_000n,
      blockSlot: predecessor.header.blockSlot + 40n,
    },
  };
};

type Opened = Awaited<ReturnType<typeof openPredecessor>>;

const EVENT_POLICY_ID = "ab".repeat(28);
const eventHistory = {
  policyId: EVENT_POLICY_ID,
  address: credentialToAddress("Custom", {
    type: "Script",
    hash: EVENT_POLICY_ID,
  }),
  retentionAddress: "retention-fixture",
  inlineLimitBytes: 512n,
};

/**
 * An inline Order read back through the SDK's shared history reader: the
 * builders authenticate exactly this shape, and the committee never sees L1.
 */
const readEventOrder = async (input: {
  eventId: SDK.OutputReference;
  payload: SDK.EventHistoryPayload;
  inclusionTime: bigint;
  owner: string;
}) => {
  const key = await Effect.runPromise(SDK.eventHistoryKey(input.eventId));
  const root: UTxO = {
    txHash: "01".repeat(32),
    outputIndex: 0,
    address: eventHistory.address,
    assets: { lovelace: 3_000_000n, [EVENT_POLICY_ID]: 1n },
    datum: Data.to(
      {
        position: "Root",
        next: key,
        protected_until: 0n,
        payload: "RootContent",
      },
      SDK.EventHistoryNode,
    ),
  };
  const order: UTxO = {
    txHash: "02".repeat(32),
    outputIndex: 0,
    address: eventHistory.address,
    assets: { lovelace: 10_000_000n, [EVENT_POLICY_ID + key]: 1n },
    datum: replacePlutusConstrFieldCbor(
      Data.to(
        {
          position: { Key: [key] },
          next: null,
          protected_until: 0n,
          payload: {
            Order: {
              facts: {
                event_id: input.eventId,
                inclusion_time: input.inclusionTime,
                location: { Inline: { payload: input.payload } },
                structural_lovelace: 2_000_000n,
                structural_refund_key: input.owner,
              },
            },
          },
        },
        SDK.EventHistoryNode,
      ),
      [3, 0, 2, 0],
      canonical(Data.to(input.payload, SDK.EventHistoryPayload)),
    ),
  };
  return { root, order };
};

const ownerAddress = (owner: string): SDK.AddressData => ({
  paymentCredential: { PublicKeyCredential: [owner] },
  stakeCredential: null,
});

const stagedDeposit = async (opened: Opened): Promise<StagedHistoryEvent> => {
  const utxos = await readEventOrder({
    eventId: { transactionId: "34".repeat(32), outputIndex: 0n },
    payload: {
      DepositPayload: {
        event: {
          id: { transactionId: "34".repeat(32), outputIndex: 0n },
          info: {
            l2_address: ownerAddress(opened.owner),
            l2_network_id: opened.predecessor.header.expectedNetworkId,
            l2_datum: null,
          },
        },
      },
    },
    inclusionTime: opened.predecessor.header.endTime + 1_000n,
    owner: opened.owner,
  });
  const [order] = await Effect.runPromise(
    SDK.utxosToDepositUTxOs([utxos.root, utxos.order], [], eventHistory),
  );
  return { order: order!, policyId: EVENT_POLICY_ID };
};

/** A withdrawal of the owner's first output, signed by the owner. */
const stagedWithdrawal = async (
  opened: Opened,
  ordinal: number,
): Promise<StagedHistoryEvent> => {
  const body = journeyWithdrawalBody({
    predecessor: opened.predecessor,
    owner: opened.owner,
  });
  const id = { transactionId: "35".repeat(32), outputIndex: BigInt(ordinal) };
  const utxos = await readEventOrder({
    eventId: id,
    payload: {
      WithdrawalPayload: {
        event: {
          id,
          info: {
            body,
            signature: SDK.signWithdrawalBody(opened.ownerKey, body),
            validity: "WithdrawalIsValid",
          },
        },
        refund_address: ownerAddress(opened.owner),
        refund_datum: "NoDatum",
      },
    },
    inclusionTime: opened.predecessor.header.endTime + 1_000n + BigInt(ordinal),
    owner: opened.owner,
  });
  const [order] = await Effect.runPromise(
    SDK.utxosToWithdrawalUTxOs([utxos.root, utxos.order], [], eventHistory),
  );
  return { order: order!, policyId: EVENT_POLICY_ID };
};

const ORDER_KEY = { transactionId: "a1".repeat(32), outputIndex: 0n };

const includes = <T extends string>(
  categories: readonly T[],
  category: string,
): category is T => (categories as readonly string[]).includes(category);

/** Build the same fault payload the family's journey fixture commits. */
const buildFaultBlock = async (
  category: JourneyCategory,
  opened: Opened,
): Promise<JourneyBlock> => {
  const { input } = opened;
  if (includes(JOURNEY_TRANSACTION_CATEGORIES, category))
    return buildJourneyTransactionFault({ ...input, category });
  if (includes(JOURNEY_FORCED_TRANSACTION_CATEGORIES, category))
    return buildJourneyForcedTransaction({
      ...input,
      category,
      orderKey: ORDER_KEY,
    });
  if (includes(JOURNEY_TRANSACTION_SOURCE_CATEGORIES, category))
    return buildJourneyTransactionSourceFault({ ...input, category });
  if (includes(JOURNEY_SCRIPT_FORCED_CATEGORIES, category))
    return buildJourneyScriptForcedFault({
      ...input,
      category,
      orderKey: ORDER_KEY,
    });
  if (includes(JOURNEY_HISTORY_TRANSACTION_CATEGORIES, category))
    return buildJourneyHistoryTransaction({ ...input, category });
  const timed = {
    predecessor: input.predecessor,
    operatorVkey: input.operatorVkey,
    endTime: input.endTime,
    blockSlot: input.blockSlot,
  };
  switch (category) {
    case "validationTraceDispute":
      return buildJourneyValidationTraceFault(input);
    case "transitionTrace": {
      // The published-deposit fixture's fault: one real deposit whose
      // transition the operator commits with a wrong post-state.
      const { order } = await stagedDeposit(opened);
      if (order.kind !== "Deposit")
        throw new Error("Staged deposit resolved to a withdrawal Order");
      return depositEventsRetainedBlock({
        operatorVkey: input.operatorVkey,
        startTime: input.predecessor.header.endTime,
        endTime: input.endTime,
        blockSlot: input.blockSlot,
        prevHeaderHash: input.predecessor.headerHash,
        prevUtxosRoot: input.predecessor.header.utxosRoot,
        priorLedger: journeyLedgerPairs(input.predecessor.ledger),
        events: [
          {
            event: order.event,
            originalAssets: order.originalAssets,
            honest: false,
          },
        ],
      });
    }
    case "fabricatedDeposit":
      return buildJourneyFabricatedDeposit({
        ...timed,
        deposit: await stagedDeposit(opened),
      });
    case "fabricatedWithdrawal":
    case "withdrawalMistag":
      return buildJourneyWithdrawalEvent({
        ...timed,
        category,
        withdrawals: [await stagedWithdrawal(opened, 0)],
      });
    case "doubleWithdraw":
      return buildJourneyWithdrawalEvent({
        ...timed,
        category,
        withdrawals: [
          await stagedWithdrawal(opened, 0),
          await stagedWithdrawal(opened, 1),
        ],
      });
    case "withdrawnInput":
    case "withdrawnReferenceInput":
      return buildJourneyWithdrawnTransaction({
        ...timed,
        category,
        withdrawal: await stagedWithdrawal(opened, 0),
        ledgerOwnerSeedPhrase: input.ledgerOwnerSeedPhrase,
      });
    default:
      throw new Error(`No committee classification for ${String(category)}`);
  }
};

describe("honest DA committee over the journey fault payloads", () => {
  it("classifies every registered journey fixture", () => {
    expect(
      JOURNEY_FIXTURE_CANDIDATES.map(({ category }) => category).sort(),
    ).toEqual(Object.keys(COMMITTEE_ACCEPTANCE).sort());
  });

  it("signs an honest block built on the same predecessor", async () => {
    const opened = await openPredecessor();
    expect(
      await committeeVerdict(
        await buildJourneyTransactionControl(opened.input),
      ),
    ).toBe("accepted");
  });

  it.each(Object.entries(COMMITTEE_ACCEPTANCE))(
    "%s fault payload: %s",
    async (category, verdict) => {
      const opened = await openPredecessor();
      const block = await buildFaultBlock(category as JourneyCategory, opened);
      expect(await committeeVerdict(block)).toBe(verdict);
    },
    60_000,
  );
});
