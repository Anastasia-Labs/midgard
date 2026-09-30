import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import {
  type Assets,
  CML,
  Data,
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccountFromPrivateKey,
  paymentCredentialOf,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";
import { expect } from "vitest";

import { TEST_AVAILABILITY_PARAMETERS } from "./availability-challenge.js";
import {
  availabilityCommitReferenceScriptTargets,
  type AvailabilityFixtureOptions,
  availabilityRedeemerScript,
  availabilityReferenceScriptTargets,
  availabilityScriptNames,
  describeExpectation,
} from "./availability-challenge-emulator.availability-redeemer-script.js";
import {
  AVAILABILITY_COLLATERAL_COIN_LOVELACE,
  AVAILABILITY_DEFAULT_POOL_LOVELACE,
  AVAILABILITY_EMULATOR_PARAMETERS,
  AVAILABILITY_QUEUE_NODE_LOVELACE,
  AVAILABILITY_TIMING,
  type AvailabilityMeasurement,
  type AvailabilityRefusal,
  type AvailabilityRefusalExpectation,
  createAvailabilityEmulatorLucid,
  evaluationSequence,
  lastEvaluationFailure,
  measureAvailabilityTransaction,
  parseAvailabilityEvaluationFailure,
  sameOutRef,
} from "./availability-challenge-emulator.measure-availability-transaction.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";

let registeredScriptNames: Readonly<Record<string, string>> = {};

/** Refusals `assertAvailabilityRefusal` confirmed so far; the fit report counts them. */
export let confirmedRefusalCount = 0;

/**
 * H9: asserts `attempt` is refused by local evaluation, and that the failing
 * redeemer runs the expected script under the expected purpose (or that the
 * evaluator's message matches `trace`). A refusal elsewhere, or no refusal,
 * fails the test. `attempt` is a promise the caller already started (an SDK
 * build, or `TxBuilder.complete`), or a hand-built `TxBuilder` this helper
 * completes without coin selection.
 */
export const assertAvailabilityRefusal = async (
  attempt: Promise<unknown> | TxBuilder,
  expected: AvailabilityRefusalExpectation,
  /** Contract names to script hashes; defaults to the latest fixture's. */
  names: Readonly<Record<string, string>> = registeredScriptNames,
): Promise<AvailabilityRefusal> => {
  const before = evaluationSequence;
  const promise =
    attempt instanceof Promise
      ? attempt
      : attempt.complete({ coinSelection: false, localUPLCEval: true });
  let rejection: unknown;
  try {
    await promise;
  } catch (cause) {
    rejection = cause;
  }
  if (rejection === undefined)
    throw new Error(
      `Expected an evaluation refusal (${describeExpectation(expected)}), but the transaction completed`,
    );
  const failure = lastEvaluationFailure;
  if (failure === undefined || failure.sequence <= before)
    throw new Error(
      `Expected an evaluation refusal (${describeExpectation(expected)}), but the build failed before evaluation: ${rejection instanceof Error ? rejection.message : String(rejection)}`,
    );
  const located = parseAvailabilityEvaluationFailure(failure.diagnosis);
  const scriptHash = availabilityRedeemerScript(
    failure.tx,
    failure.utxos,
    located.purpose,
    located.index,
  );
  const refusal = { ...located, scriptHash, message: failure.diagnosis };
  if ("trace" in expected) {
    expect(`${failure.diagnosis}\n${failure.message}`).toMatch(expected.trace);
    confirmedRefusalCount += 1;
    return refusal;
  }
  const expectedHash = names[expected.script] ?? expected.script;
  const label = (hash: string | undefined) => {
    const name = Object.entries(names).find(([, h]) => h === hash)?.[0];
    return `${hash ?? "unknown"}${name ? ` (${name})` : ""}`;
  };
  expect(
    { purpose: located.purpose, script: label(scriptHash) },
    `refusal must come from the expected check; evaluator said: ${failure.diagnosis}`,
  ).toEqual({ purpose: expected.purpose, script: label(expectedHash) });
  if (expected.index !== undefined) expect(located.index).toBe(expected.index);
  confirmedRefusalCount += 1;
  return refusal;
};

/**
 * The fixture body. With `emptyQueue`, no block is seeded (the root's `next`
 * is `Empty`, so `target` is undefined and `payload`, `commitment` and
 * `queueUnit` name no on-chain block), and the fixture also holds what a real
 * `CommitBlockHeader` needs: the scheduler naming the responder, the
 * responder's active-operator node, the commit reference scripts and the
 * registered commit yield. Only `createAvailabilityCommitFixture` sets it.
 */
export const createFixture = async (
  payloadBytes: number,
  descendantCount: number,
  headerEndTimeLeadMs: number,
  options: AvailabilityFixtureOptions,
  emptyQueue: boolean,
) => {
  if (emptyQueue && descendantCount !== 0)
    throw new Error("An empty-queue fixture seeds no descendants");
  const parameters = TEST_AVAILABILITY_PARAMETERS;
  const oneShotHolder = generateEmulatorAccountFromPrivateKey({
    lovelace: 2_000_000_000n,
  });
  const responder = generateEmulatorAccountFromPrivateKey({
    lovelace: 100_000_000_000n,
  });
  const challenger = generateEmulatorAccountFromPrivateKey({
    lovelace: 50_000_000_000n,
  });
  const publisher = generateEmulatorAccountFromPrivateKey({
    lovelace: 100_000_000_000n,
  });
  const preliminary = new Emulator(
    [publisher],
    AVAILABILITY_EMULATOR_PARAMETERS,
  );
  const preliminaryLucid = await createAvailabilityEmulatorLucid(preliminary);
  preliminaryLucid.selectWallet.fromPrivateKey(publisher.privateKey);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(
    preliminaryLucid,
    preliminary.now(),
  );
  const hubOneShot = { txHash: "00".repeat(32), outputIndex: 0 };
  const contracts = await loadRealMidgardContractsForTest(
    hubOneShot,
    authPolicy,
  );
  const scriptNames = availabilityScriptNames(contracts);
  registeredScriptNames = scriptNames;
  const now = preliminary.now();
  const challengerKey = paymentCredentialOf(challenger.address).hash;
  const responderKey = paymentCredentialOf(responder.address).hash;
  const committeeKeys = [
    CML.PrivateKey.generate_ed25519(),
    CML.PrivateKey.generate_ed25519(),
  ].sort((a, b) =>
    Buffer.compare(
      Buffer.from(a.to_public().to_raw_bytes()),
      Buffer.from(b.to_public().to_raw_bytes()),
    ),
  );
  const committee = committeeKeys
    .map((key) => Buffer.from(key.to_public().to_raw_bytes()).toString("hex"))
    .join("");
  const daParamsDatum: SDK.DaParamsDatum = {
    committee,
    committee_signers_hash: Buffer.from(
      blake2b(Buffer.from(committee, "hex"), { dkLen: 32 }),
    ).toString("hex"),
    da_threshold: 2n,
    owners: [challengerKey, responderKey].sort(),
    update_threshold: 2n,
  };
  const stateQueueNode: SDK.StateQueueNode = {
    proven_fraud: null,
    header: {
      prevUtxosRoot: h32(0x01),
      utxosRoot: h32(0x02),
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
      startTime: BigInt(now - 1_000),
      endTime: BigInt(now + headerEndTimeLeadMs),
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 44n,
      minFeeB: 155381n,
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      operatorVkey: responderKey,
      protocolVersion: 1n,
    },
    da_attestation: SDK.NO_DA_ATTESTATION,
  };
  await Effect.runPromise(
    SDK.validateHeaderTransitionCommitmentsProgram(stateQueueNode.header),
  );
  const headerHash = await Effect.runPromise(
    SDK.hashBlockHeader(stateQueueNode.header),
  );
  const queueDatum: SDK.LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: SDK.castStateQueueNodeToData(
      stateQueueNode,
    ) as SDK.LinkedListNodeView["data"],
  };
  const descendants: { hash: string; datum: SDK.LinkedListNodeView }[] = [];
  let previousHash = headerHash;
  for (let i = 0; i < descendantCount; i++) {
    const node: SDK.StateQueueNode = {
      ...stateQueueNode,
      header: {
        ...stateQueueNode.header,
        prevHeaderHash: previousHash,
        blockSlot: BigInt(i + 1),
      },
    };
    const hash = await Effect.runPromise(SDK.hashBlockHeader(node.header));
    descendants.push({
      hash,
      datum: {
        key: { Key: { key: hash } },
        next: "Empty",
        data: SDK.castStateQueueNodeToData(
          node,
        ) as SDK.LinkedListNodeView["data"],
      },
    });
    previousHash = hash;
  }
  if (descendants.length > 0)
    queueDatum.next = { Key: { key: descendants[0]!.hash } };
  for (let i = 0; i < descendants.length - 1; i++)
    descendants[i]!.datum.next = { Key: { key: descendants[i + 1]!.hash } };
  const rootDatum: SDK.LinkedListNodeView = {
    key: "Empty",
    next: emptyQueue ? "Empty" : { Key: { key: headerHash } },
    data: SDK.castConfirmedStateToData(
      SDK.makeGenesisConfirmedState(BigInt(now - 2_000)),
    ) as SDK.LinkedListNodeView["data"],
  };
  const hubDatum = await Effect.runPromise(SDK.makeHubOracleDatum(contracts));
  const queueUnit =
    contracts.stateQueue.policyId +
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
    headerHash;
  const rootUnit =
    contracts.stateQueue.policyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME;
  const hubUnit = contracts.hubOracle.policyId + SDK.HUB_ORACLE_ASSET_NAME;
  const paramsUnit =
    contracts.daParamsGovernor.policyId + SDK.DA_PARAMS_ASSET_NAME;
  const lockUnit = SDK.correctionLockUnit(contracts.hubOracle.policyId);
  const poolUnit = SDK.daBondPoolUnit(contracts.daBondPool.policyId);
  const genesis = (
    address: string,
    assets: Assets,
    datum?: string,
  ): EmulatorAccount => ({
    seedPhrase: "",
    privateKey: "",
    address,
    assets,
    ...(datum === undefined ? {} : { outputData: { inline: datum } }),
  });
  const seededPool =
    options.seedPool === false
      ? undefined
      : {
          lovelace:
            options.seedPool?.lovelace ?? AVAILABILITY_DEFAULT_POOL_LOVELACE,
          datum: options.seedPool?.datum ?? ("Bonded" as const),
        };
  const emulator = new Emulator(
    [
      // Output 0 is the hub one-shot out-reference; see the doc comment.
      oneShotHolder,
      responder,
      challenger,
      publisher,
      genesis(responder.address, {
        lovelace: AVAILABILITY_COLLATERAL_COIN_LOVELACE,
      }),
      genesis(challenger.address, {
        lovelace: AVAILABILITY_COLLATERAL_COIN_LOVELACE,
      }),
      genesis(
        contracts.hubOracle.spendingScriptAddress,
        { lovelace: 20_000_000n, [hubUnit]: 1n },
        Data.to(hubDatum, SDK.HubOracleDatum),
      ),
      genesis(
        contracts.daParamsGovernor.spendingScriptAddress,
        { lovelace: 3_000_000n, [paramsUnit]: 1n },
        Data.to(daParamsDatum, SDK.DaParamsDatum),
      ),
      ...(emptyQueue
        ? []
        : [
            genesis(
              contracts.stateQueue.spendingScriptAddress,
              { lovelace: AVAILABILITY_QUEUE_NODE_LOVELACE, [queueUnit]: 1n },
              SDK.encodeLinkedListNodeView(queueDatum),
            ),
          ]),
      genesis(
        contracts.stateQueue.spendingScriptAddress,
        { lovelace: AVAILABILITY_QUEUE_NODE_LOVELACE, [rootUnit]: 1n },
        SDK.encodeLinkedListNodeView(rootDatum),
      ),
      ...descendants.map(({ hash, datum }) =>
        genesis(
          contracts.stateQueue.spendingScriptAddress,
          {
            lovelace: AVAILABILITY_QUEUE_NODE_LOVELACE,
            [contracts.stateQueue.policyId +
            SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
            hash]: 1n,
          },
          SDK.encodeLinkedListNodeView(datum),
        ),
      ),
      genesis(
        contracts.correctionLock.spendingScriptAddress,
        { lovelace: 3_000_000n, [lockUnit]: 1n },
        Data.to("Idle", SDK.CorrectionLockDatum),
      ),
      ...(seededPool === undefined
        ? []
        : [
            genesis(
              contracts.daBondPool.spendingScriptAddress,
              { lovelace: seededPool.lovelace, [poolUnit]: 1n },
              SDK.encodeDaBondPoolDatum(seededPool.datum),
            ),
          ]),
      ...(emptyQueue
        ? [
            // The scheduler names the responder as the active operator.
            genesis(
              contracts.scheduler.spendingScriptAddress,
              {
                lovelace: 3_000_000n,
                [contracts.scheduler.policyId + SDK.SCHEDULER_ASSET_NAME]: 1n,
              },
              Data.to(
                {
                  ActiveOperator: {
                    operator: responderKey,
                    start_time: BigInt(now),
                  },
                },
                SDK.SchedulerDatum,
              ),
            ),
            // The responder's active-operator node, no bond hold yet.
            genesis(
              contracts.activeOperators.spendingScriptAddress,
              {
                lovelace: 5_000_000n,
                [contracts.activeOperators.policyId +
                SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX +
                responderKey]: 1n,
              },
              SDK.encodeLinkedListNodeView({
                key: { Key: { key: responderKey } },
                next: "Empty",
                data: SDK.castActiveOperatorDatumToData({
                  bond_unlock_time: null,
                  inactivity_strikes: 0n,
                }) as SDK.LinkedListNodeView["data"],
              }),
            ),
          ]
        : []),
    ],
    AVAILABILITY_EMULATOR_PARAMETERS,
  );
  // Created at emulator genesis, so zeroTime is genesis time and zeroSlot 0:
  // never create another Lucid mid-test (its zeroTime would be "now").
  const lucid = await createAvailabilityEmulatorLucid(emulator);
  const publishingLucid = await createAvailabilityEmulatorLucid(emulator);
  lucid.selectWallet.fromPrivateKey(responder.privateKey);
  publishingLucid.selectWallet.fromPrivateKey(publisher.privateKey);
  const measurements: AvailabilityMeasurement[] = [];
  const refusalsAtCreation = confirmedRefusalCount;
  const submit = async (
    name: string,
    builder: TxBuilder,
    coinSelection = false,
    /** Private keys that sign beside the selected wallet. */
    extraSigners: readonly string[] = [],
  ) => {
    const unsigned = await builder
      .complete({ localUPLCEval: true, coinSelection })
      .catch((cause: unknown) => {
        throw new Error(`Availability stage ${name}: ${String(cause)}`, {
          cause,
        });
      });
    let signing = unsigned.sign.withWallet();
    for (const key of extraSigners) signing = signing.sign.withPrivateKey(key);
    const signed = await signing.complete();
    const referenceInputs = CML.Transaction.from_cbor_hex(signed.toCBOR())
      .body()
      .reference_inputs();
    const refs = await lucid.utxosByOutRef(
      Array.from({ length: referenceInputs?.len() ?? 0 }, (_, i) => {
        const input = referenceInputs!.get(i);
        return {
          txHash: input.transaction_id().to_hex(),
          outputIndex: Number(input.index()),
        };
      }),
    );
    measurements.push(
      measureAvailabilityTransaction(name, signed.toCBOR(), refs),
    );
    const hash = await signed.submit();
    emulator.awaitBlock(1);
    return lucid.utxosByOutRef(
      Array.from(
        {
          length: CML.Transaction.from_cbor_hex(signed.toCBOR())
            .body()
            .outputs()
            .len(),
        },
        (_, outputIndex) => ({ txHash: hash, outputIndex }),
      ),
    );
  };
  const references = new Map<string, UTxO>();
  for (const target of [
    ...availabilityReferenceScriptTargets(contracts),
    ...(emptyQueue ? availabilityCommitReferenceScriptTargets(contracts) : []),
  ]) {
    const { tx, layout } = await Effect.runPromise(
      SDK.completeReferenceScriptPublicationTxProgram({
        lucid: publishingLucid,
        selectedFundingInputs: SDK.selectReferenceScriptFundingUtxos(
          await publishingLucid.wallet().getUtxos(),
          SDK.referenceScriptPublicationFundingTarget(1),
        ),
        walletAddress: publisher.address,
        referenceScriptsAddress: publisher.address,
        missingTargets: [target],
        authPolicy,
      }),
    );
    const signed = await tx.sign.withWallet().complete();
    const measurement = measureAvailabilityTransaction(
      `publish ${target.name}`,
      signed.toCBOR(),
    );
    expect(measurement.signedBytes).toBeLessThanOrEqual(15_872);
    measurements.push(measurement);
    const hash = await signed.submit();
    emulator.awaitBlock(1);
    const local = layout.localReferenceOutputs.get(target.name);
    if (!local) throw new Error(`Missing published role ${target.name}`);
    const [utxo] = await lucid.utxosByOutRef([
      { txHash: hash, outputIndex: local.outputIndex },
    ]);
    if (!utxo) throw new Error(`Missing published reference ${target.name}`);
    references.set(target.name, utxo);
  }
  const reference = (name: string): UTxO => {
    const utxo = references.get(name);
    if (!utxo) throw new Error(`Unavailable reference ${name}`);
    return utxo;
  };
  let registrations = lucid.newTx();
  const rewardAddresses = [
    ...Object.values(contracts.availabilityChallenge.yields),
    contracts.stateQueue.yields.unavailableTimeout,
    contracts.stateQueue.yields.merge,
    ...(emptyQueue ? [contracts.stateQueue.yields.commit] : []),
  ].map(({ withdrawalScript }) =>
    SDK.scriptRewardAddress("Preprod", withdrawalScript),
  );
  for (const address of rewardAddresses)
    registrations = registrations.register.Stake(address);
  await submit("register availability reward credentials", registrations, true);
  for (const address of rewardAddresses)
    expect((await lucid.rewardAccountAt(address)).registered).toBe(true);
  const [hubOracleRefInput] = await lucid.utxosAtWithUnit(
    contracts.hubOracle.spendingScriptAddress,
    hubUnit,
  );
  const [daParamsUtxo] = await lucid.utxosAtWithUnit(
    contracts.daParamsGovernor.spendingScriptAddress,
    paramsUnit,
  );
  const [queueUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    queueUnit,
  );
  const [rootUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    rootUnit,
  );
  const [correctionLockUtxo] = await lucid.utxosAtWithUnit(
    contracts.correctionLock.spendingScriptAddress,
    lockUnit,
  );
  const [hubOneShotUtxo] = await lucid.utxosByOutRef([hubOneShot]);
  if (
    !hubOracleRefInput ||
    !daParamsUtxo ||
    (!queueUtxo && !emptyQueue) ||
    !rootUtxo ||
    !correctionLockUtxo ||
    !hubOneShotUtxo
  )
    throw new Error("Incomplete genesis fixture");
  const payload = Uint8Array.from(
    { length: payloadBytes },
    (_, i) => (i * 17 + 3) % 256,
  );
  const commitment = SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: contracts.hubOracle.policyId,
    headerHash,
    payload,
    responseGeometry: SDK.availabilityResponseGeometry({
      chunkByteLength: Number(parameters.response_geometry.chunk_byte_length),
      trancheByteLength: Number(
        parameters.response_geometry.tranche_byte_length,
      ),
      maxTrancheCount: Number(parameters.response_geometry.max_tranche_count),
    }),
  });
  const target: SDK.DaAttestationStateQueueTarget | undefined =
    queueUtxo === undefined
      ? undefined
      : {
          headerHash,
          stateQueueNode,
          stateQueueUtxo: {
            utxo: queueUtxo,
            datum: queueDatum,
            assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
          },
        };
  const daReferences: SDK.DaAttestationReferenceScripts = {
    daAttestationMinting: reference("da-attestation minting"),
    daAttestationSpending: reference("da-attestation spending"),
    stateQueueMinting: reference("state-queue minting"),
    stateQueueSpending: reference("state-queue spending"),
  };
  const poolReferences = {
    daBondPoolMinting: reference("da-bond-pool minting"),
    daBondPoolSpending: reference("da-bond-pool spending"),
  };

  // --- pool helpers --------------------------------------------------------

  /** The one live pool UTxO (fails closed when absent or ambiguous). */
  const getPool = async (): Promise<UTxO> =>
    (
      await SDK.fetchDaBondPool(lucid, {
        policyId: contracts.daBondPool.policyId,
        address: contracts.daBondPool.spendingScriptAddress,
      })
    ).utxo;
  /** Runs `body` with `privateKey`'s wallet selected, then restores `after`. */
  const asWallet = async <A>(
    privateKey: string,
    body: () => Promise<A>,
    after: string = responder.privateKey,
  ): Promise<A> => {
    lucid.selectWallet.fromPrivateKey(privateKey);
    try {
      return await body();
    } finally {
      lucid.selectWallet.fromPrivateKey(after);
    }
  };
  const poolOutput = (outputs: readonly UTxO[]) => {
    const pool = outputs.find((u) => u.assets[poolUnit] === 1n);
    if (!pool) throw new Error("Transaction produced no pool output");
    return pool;
  };
  /** Advances the emulator clock to at least `ms` (one-second slots). */
  const advanceToMs = (ms: bigint | number) => {
    const deficit = Number(ms) - emulator.now();
    if (deficit > 0) emulator.awaitSlot(Math.ceil(deficit / 1_000));
  };
  const quorum = {
    signerKeyHashes: daParamsDatum.owners,
    // The owners are the challenger and the responder: the responder's
    // wallet signs, the challenger's key signs beside it.
    extraSigners: [challenger.privateKey],
  };
  /**
   * The real `InitPool`: spends the hub one-shot out-reference (the pool's
   * `init_ref`) and mints the pool NFT to `Script(pool policy)` with a
   * `Bonded` datum. Only meaningful in a `seedPool: false` fixture.
   */
  const initPoolReal = async (
    lovelace: bigint = AVAILABILITY_DEFAULT_POOL_LOVELACE,
  ): Promise<UTxO> =>
    asWallet(oneShotHolder.privateKey, async () => {
      const tx = await Effect.runPromise(
        SDK.buildInitDaBondPoolTxProgram(lucid, {
          poolValidator: contracts.daBondPool,
          parameters,
          initUtxo: hubOneShotUtxo,
          lovelace,
          referenceScripts: {
            daBondPoolMinting: poolReferences.daBondPoolMinting,
          },
        }),
      );
      return poolOutput(await submit("init DA bond pool", tx, true));
    });
  /** `TopUp`: the selected wallet adds `amount` to the pool. */
  const topUpPool = async (
    amount: bigint,
    options: { skipMinimumPrecheck?: true } = {},
  ): Promise<UTxO> => {
    const tx = await Effect.runPromise(
      SDK.buildTopUpDaBondPoolTxProgram(lucid, {
        poolValidator: contracts.daBondPool,
        parameters,
        pool: { utxo: await getPool() },
        amount,
        referenceScripts: {
          daBondPoolSpending: poolReferences.daBondPoolSpending,
        },
        ...options,
      }),
    );
    return poolOutput(await submit(`top up DA bond pool ${amount}`, tx, true));
  };
  const quorumConfig = async () => ({
    poolValidator: contracts.daBondPool,
    parameters,
    pool: { utxo: await getPool() },
    daParamsUtxo,
    signerKeyHashes: quorum.signerKeyHashes,
    referenceScripts: {
      daBondPoolSpending: poolReferences.daBondPoolSpending,
    },
  });
  /**
   * `BeginWithdraw` under the owner quorum; the datum's `unlock_at` is the
   * slot-aligned `validTo - 1 + da_bond_withdraw_delay_ms`.
   */
  const beginPoolWithdraw = async (
    validity: { validFrom?: bigint; validTo?: bigint } = {},
  ): Promise<{ pool: UTxO; unlockAt: bigint }> =>
    asWallet(responder.privateKey, async () => {
      const validFrom = validity.validFrom ?? BigInt(emulator.now());
      const tx = await Effect.runPromise(
        SDK.buildBeginDaBondPoolWithdrawTxProgram(lucid, {
          ...(await quorumConfig()),
          withdrawDelayMs: AVAILABILITY_TIMING.daBondWithdrawDelayMs,
          validity: {
            validFrom,
            validTo: validity.validTo ?? validFrom + 60_000n,
          },
        }),
      );
      const pool = poolOutput(
        await submit(
          "begin DA bond pool withdraw",
          tx,
          true,
          quorum.extraSigners,
        ),
      );
      const datum = SDK.decodeDaBondPoolDatum(pool.datum!);
      if (datum === "Bonded") throw new Error("BeginWithdraw left pool Bonded");
      return { pool, unlockAt: datum.Withdrawing.unlock_at };
    });
  const cancelPoolWithdraw = async (): Promise<UTxO> =>
    asWallet(responder.privateKey, async () => {
      const tx = await Effect.runPromise(
        SDK.buildCancelDaBondPoolWithdrawTxProgram(lucid, await quorumConfig()),
      );
      return poolOutput(
        await submit(
          "cancel DA bond pool withdraw",
          tx,
          true,
          quorum.extraSigners,
        ),
      );
    });
  /**
   * `CompleteWithdraw { amount }` at or after `unlock_at`; pays `amount` to
   * `destination` (default the responder).
   */
  const completePoolWithdraw = async (
    amount: bigint,
    options: {
      destination?: string;
      validFrom?: bigint;
      skipUnlockPrecheck?: true;
    } = {},
  ): Promise<UTxO> =>
    asWallet(responder.privateKey, async () => {
      const tx = await Effect.runPromise(
        SDK.buildCompleteDaBondPoolWithdrawTxProgram(lucid, {
          ...(await quorumConfig()),
          amount,
          destination: options.destination ?? responder.address,
          validity: { validFrom: options.validFrom ?? BigInt(emulator.now()) },
          ...(options.skipUnlockPrecheck
            ? { skipUnlockPrecheck: true as const }
            : {}),
        }),
      );
      return poolOutput(
        await submit(
          `complete DA bond pool withdraw ${amount}`,
          tx,
          true,
          quorum.extraSigners,
        ),
      );
    });
  /**
   * Plain-ADA collateral coins of the selected wallet, none of `exclude`.
   * Each covers the largest DA availability collateral alone.
   */
  const collateralInputs = async (
    exclude: readonly UTxO[] = [],
  ): Promise<UTxO[]> => {
    const coins = (await lucid.wallet().getUtxos()).filter(
      (u) =>
        u.assets.lovelace === AVAILABILITY_COLLATERAL_COIN_LOVELACE &&
        Object.keys(u.assets).length === 1 &&
        !u.datum &&
        !u.datumHash &&
        !u.scriptRef &&
        !exclude.some((e) => sameOutRef(e, u)),
    );
    if (coins.length === 0)
      throw new Error(
        `The selected wallet holds no ${AVAILABILITY_COLLATERAL_COIN_LOVELACE}-lovelace collateral coin`,
      );
    return coins;
  };

  return {
    emulator,
    lucid,
    contracts,
    parameters,
    timing: AVAILABILITY_TIMING,
    scriptNames,
    authPolicy,
    oneShotHolder,
    hubOneShot,
    responder,
    challenger,
    challengerKey,
    responderKey,
    committeeKeys,
    daParamsDatum,
    daParamsUtxo,
    hubOracleRefInput,
    correctionLockUtxo,
    rootUtxo,
    rootDatum,
    rootUnit,
    queueUnit,
    poolUnit,
    target,
    payload,
    commitment,
    daReferences,
    poolReferences,
    reference,
    measurements,
    submit,
    getPool,
    initPoolReal,
    topUpPool,
    beginPoolWithdraw,
    cancelPoolWithdraw,
    completePoolWithdraw,
    advanceToMs,
    collateralInputs,
    refusalsAtCreation,
  };
};
