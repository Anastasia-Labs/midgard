import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { type LucidDataSchema } from "@al-ft/midgard-core/lucid-data";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  AddressData,
  addressDataFromBech32,
  castConfirmedStateToData,
  EMPTY_HEADER_TRANSITION_COMMITMENTS,
  EMPTY_MERKLE_TREE_ROOT,
  type LinkedListNodeView,
  makeGenesisConfirmedState,
  updateLatestBlocksDatumAndGetTheNewHeaderProgram,
} from "../src/index.js";
import {
  alwaysAuthenticated,
  alwaysScript,
  applyAllBlueprintParamsToScript,
  type Blueprint,
  makeAuthenticatedValidator,
  makeMintingValidator,
  makeSpendingValidator,
  type StateQueueTestContracts,
} from "./state-queue.state-queue-operator-funding-inputs.js";

export const buildTestContracts = async (
  realBlueprint: Blueprint,
  alwaysBlueprint: Blueprint,
): Promise<StateQueueTestContracts> => {
  const hubOracleScript = alwaysScript(alwaysBlueprint, "hub_oracle", "mint");
  const base = {
    hubOracle: makeAuthenticatedValidator(hubOracleScript, hubOracleScript),
    daAttestation: alwaysAuthenticated(alwaysBlueprint, "state_queue"),
    scheduler: alwaysAuthenticated(alwaysBlueprint, "scheduler"),
    activeOperators: alwaysAuthenticated(alwaysBlueprint, "active_operators"),
    retiredOperators: alwaysAuthenticated(alwaysBlueprint, "retired_operators"),
    fraudProof: alwaysAuthenticated(alwaysBlueprint, "fraud_proof"),
    settlement: alwaysAuthenticated(alwaysBlueprint, "settlement"),
  };
  const activeOperatorsAddressData = await Effect.runPromise(
    addressDataFromBech32(base.activeOperators.spendingScriptAddress).pipe(
      Effect.map((addressData) => Data.from(Data.to(addressData, AddressData))),
    ),
  );
  const computationThread = alwaysAuthenticated(alwaysBlueprint, "payout");
  const availabilityChallenge = alwaysAuthenticated(
    alwaysBlueprint,
    "escape_hatch",
  );
  const correctionLock = makeSpendingValidator(
    applyAllBlueprintParamsToScript(
      realBlueprint,
      "correction_lock.spend.spend",
      [base.hubOracle.policyId, availabilityChallenge.policyId],
    ),
  );
  const stateQueueMintingScriptCBOR = applyAllBlueprintParamsToScript(
    realBlueprint,
    "state_queue.mint.mint",
    [
      base.hubOracle.policyId,
      correctionLock.spendingScriptHash,
      base.activeOperators.policyId,
      activeOperatorsAddressData,
      base.retiredOperators.policyId,
      base.scheduler.policyId,
      base.fraudProof.policyId,
      base.settlement.policyId,
      base.daAttestation.policyId,
      availabilityChallenge.policyId,
      base.scheduler.policyId,
    ],
  );
  const stateQueueMinting = makeMintingValidator(stateQueueMintingScriptCBOR);
  const stateQueueSpendingScriptCBOR = applyAllBlueprintParamsToScript(
    realBlueprint,
    "state_queue.spend.spend",
    [
      stateQueueMinting.policyId,
      base.daAttestation.policyId,
      availabilityChallenge.policyId,
      base.fraudProof.policyId,
    ],
  );
  const commitYield = makeSpendingValidator(
    applyAllBlueprintParamsToScript(
      realBlueprint,
      "state_queue_yields.commit.withdraw",
      [
        stateQueueMinting.policyId,
        base.hubOracle.policyId,
        correctionLock.spendingScriptHash,
        base.activeOperators.policyId,
        activeOperatorsAddressData,
        base.scheduler.policyId,
        base.daAttestation.policyId,
      ],
    ),
  );
  const fraudRemovalYield = makeSpendingValidator(
    applyAllBlueprintParamsToScript(
      realBlueprint,
      "state_queue_yields.remove_fraudulent.withdraw",
      [
        stateQueueMinting.policyId,
        base.hubOracle.policyId,
        correctionLock.spendingScriptHash,
        base.activeOperators.policyId,
        base.retiredOperators.policyId,
        base.fraudProof.policyId,
      ],
    ),
  );

  return {
    ...base,
    correctionLock,
    commitYield,
    fraudRemovalYield,
    computationThread,
    stateQueue: {
      ...stateQueueMinting,
      ...makeSpendingValidator(stateQueueSpendingScriptCBOR),
    },
  };
};

export const roundTrip = <A>(value: A, schema: LucidDataSchema): A =>
  Data.from(Data.to(value, schema), schema) as A;

const trieRootHex = (trie: Trie): string =>
  trie.hash === null || trie.hash === undefined
    ? EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(trie.hash).toString("hex");

const outputReferenceCbor = (txHash: string, outputIndex: bigint): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHash),
      outputIndex,
    ).to_cbor_bytes(),
  );

const makeNativeTx = (
  spendInputCbors: readonly Buffer[],
  fee: bigint,
): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(spendInputCbors),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

export const buildTransactionsRoot = async (): Promise<string> => {
  const tx1 = makeNativeTx(
    [outputReferenceCbor(h32(0x11), 0n), outputReferenceCbor(h32(0x22), 1n)],
    1n,
  );
  const tx2 = makeNativeTx(
    [outputReferenceCbor(h32(0x33), 0n), outputReferenceCbor(h32(0x44), 2n)],
    2n,
  );
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);

  for (const nativeTx of [tx1, tx2]) {
    await trie.insert(
      computeMidgardNativeTxId(nativeTx),
      encodeMidgardNativeTxCompact(nativeTx.compact),
    );
  }

  return trieRootHex(trie);
};

export const TWO_TRANSACTION_HEADER_COMMITMENTS = {
  transitionTraceRoot: h32(0x55),
  eventToStepRoot: h32(0x66),
  validationTracesRoot: h32(0x77),
  l2TransactionCount: 2n,
  totalEventCount: 2n,
  transitionStepCount: 2n,
  validationTraceCount: 2n,
} as const;

describe("state-queue header validation boundary", () => {
  it("validates every source root/count pair before building a header", async () => {
    const lucid = {
      wallet: () => ({
        address: async () =>
          "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58",
      }),
    } as unknown as Parameters<
      typeof updateLatestBlocksDatumAndGetTheNewHeaderProgram
    >[0];
    const latestBlocksDatum: LinkedListNodeView = {
      key: "Empty",
      next: "Empty",
      data: castConfirmedStateToData(
        makeGenesisConfirmedState(10n),
      ) as LinkedListNodeView["data"],
    };
    const sourceRoots = {
      withdrawalsRoot: h32(0x11),
      transactionsRoot: h32(0x22),
      depositsRoot: h32(0x33),
    };
    const commitments = {
      ...EMPTY_HEADER_TRANSITION_COMMITMENTS,
      transitionTraceRoot: h32(0x44),
      eventToStepRoot: h32(0x55),
      validationTracesRoot: h32(0x66),
      withdrawalCount: 1n,
      l2TransactionCount: 1n,
      depositCount: 1n,
      totalEventCount: 3n,
      transitionStepCount: 3n,
      validationTraceCount: 1n,
    };
    const updateProgram = (roots: typeof sourceRoots = sourceRoots) =>
      updateLatestBlocksDatumAndGetTheNewHeaderProgram(
        lucid,
        latestBlocksDatum,
        h32(0x77),
        roots.transactionsRoot,
        roots.depositsRoot,
        roots.withdrawalsRoot,
        commitments,
        11n,
        {
          blockSlot: 0n,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
        },
      );
    const update = (roots: typeof sourceRoots = sourceRoots) =>
      Effect.runPromise(updateProgram(roots));

    await expect(update()).resolves.toMatchObject({
      header: {
        withdrawalsRoot: sourceRoots.withdrawalsRoot,
        transactionsRoot: sourceRoots.transactionsRoot,
        depositsRoot: sourceRoots.depositsRoot,
      },
    });

    for (const [label, rootField] of [
      ["withdrawals", "withdrawalsRoot"],
      ["transactions", "transactionsRoot"],
      ["deposits", "depositsRoot"],
    ] as const) {
      const invalid = await Effect.runPromise(
        Effect.either(
          updateProgram({
            ...sourceRoots,
            [rootField]: EMPTY_MERKLE_TREE_ROOT,
          }),
        ),
      );
      expect(invalid._tag).toBe("Left");
      if (invalid._tag === "Left") {
        expect(String(invalid.left.cause)).toContain(`${label}_root`);
      }
    }
  });
});

export const isOnlyLovelace = (utxo: UTxO): boolean =>
  Object.keys(utxo.assets).every((unit) => unit === "lovelace");
