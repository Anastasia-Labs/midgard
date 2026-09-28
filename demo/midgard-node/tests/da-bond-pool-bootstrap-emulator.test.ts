import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import { type TxBuilder } from "@lucid-evolution/lucid";
import { HashMap, Logger } from "effect";
import { describe, expect, it, vi } from "vitest";

import { availabilityParametersFromExplicitEnvironment } from "../src/services/index.js";
import {
  attestStateQueueOnceProgram,
  DA_ATTESTATION_POOL_SKIP_EVENT,
  fetchDaParamsUtxo,
} from "../src/transactions/da-attestation.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  DaPayloadsDB,
  Effect,
  EMULATOR_DA_COSIGNER_SEED_PHRASE,
  type EmulatorFixture,
  fetchLatestCommittedBlock,
  initializeNodeRuntime,
  initializeProtocol,
  makeFixture,
  makeGlobalsService,
  makeLucidRuntimeService,
  Option,
  resetActiveRuntimePaths,
  retainSubmittedHeaderPayload,
  runCommitWorkerUntilSubmitted,
  runNodeCommandProgram,
  runNodeDatabaseEffect,
  SDK,
  stateQueueFetchConfig,
  submitDepositAndRefreshBarriers,
  walletFromSeed,
} from "./deposit-flow-emulator-shared.js";

/**
 * #690 AC2: the pooled DA bond from a real init backs the node's first
 * attestation, and a pool that cannot back one (drained below one bond, or
 * withdrawing) makes the node skip the round with a typed warning instead of
 * failing its attestation loop (decision E5).
 */

const BOOTSTRAP_TIMEOUT_MS = 600_000;
/** The withdraw delay the preprod-testing blueprint compiles in. */
const WITHDRAW_DELAY_MS = BigInt(
  DEPLOYMENT_PROFILES["preprod-testing"].timing.da_bond_withdraw_delay_ms,
);

type Harness = {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly globals: Awaited<ReturnType<typeof makeGlobalsService>>;
};

type CapturedLog = {
  readonly level: string;
  readonly message: string;
  readonly annotations: Readonly<Record<string, unknown>>;
};

const bootstrap = async (): Promise<Harness> => {
  await resetActiveRuntimePaths();
  await initializeNodeRuntime();
  const fixture = await makeFixture();
  await initializeProtocol(fixture);
  const lucidService = await makeLucidRuntimeService(fixture);
  const globals = await makeGlobalsService();
  return { fixture, lucidService, globals };
};

const syncClock = ({ fixture }: Harness) =>
  vi.setSystemTime(new Date(fixture.emulator.now()));

const parameters = () => availabilityParametersFromExplicitEnvironment();

const readPool = ({ fixture }: Harness) =>
  SDK.fetchDaBondPool(fixture.operatorLucid, {
    policyId: fixture.contracts.daBondPool.policyId,
    address: fixture.contracts.daBondPool.spendingScriptAddress,
    parameters: parameters(),
  });

/**
 * Runs one node attestation round for `headerHash` with a logger that records
 * every line and its annotations next to the default one.
 */
const attestRound = async (harness: Harness, headerHash: string) => {
  const logs: CapturedLog[] = [];
  const capture = Logger.make(({ logLevel, message, annotations }) => {
    logs.push({
      level: logLevel.label,
      message: (Array.isArray(message) ? message : [message])
        .map(String)
        .join(" "),
      annotations: Object.fromEntries(HashMap.toEntries(annotations)),
    });
  });
  syncClock(harness);
  const results = await runNodeCommandProgram(
    attestStateQueueOnceProgram({ headerHash }).pipe(
      Effect.provide(Logger.add(capture)),
    ),
    harness,
  );
  return { results, logs };
};

const poolSkips = (logs: readonly CapturedLog[]) =>
  logs.filter(
    (entry) => entry.annotations.event === DA_ATTESTATION_POOL_SKIP_EVENT,
  );

/** The state-queue node's `da_attestation` status for `headerHash`. */
const attestationStatus = async ({ fixture }: Harness, headerHash: string) => {
  const queue = await Effect.runPromise(
    SDK.fetchSortedStateQueueUTxOsProgram(
      fixture.operatorLucid,
      stateQueueFetchConfig(fixture.contracts),
    ),
  );
  const entry = queue.find(
    (candidate) =>
      candidate.datum.key !== "Empty" &&
      candidate.datum.key.Key.key === headerHash,
  );
  if (entry === undefined) throw new Error(`Missing header ${headerHash}`);
  const node = await Effect.runPromise(
    SDK.getStateQueueNodeFromStateQueueDatum(entry.datum),
  );
  return node.da_attestation;
};

const attestationUtxos = ({ fixture }: Harness, headerHash: string) =>
  fixture.operatorLucid.utxosAtWithUnit(
    fixture.contracts.daAttestation.spendingScriptAddress,
    SDK.daAttestationUnit(fixture.contracts.daAttestation, headerHash),
  );

/** The `commitment_hash` Apply must write for the retained payload. */
const expectedCommitmentHash = async (
  { fixture }: Harness,
  headerHash: string,
) => {
  const row = await runNodeDatabaseEffect(
    DaPayloadsDB.retrieveByHeaderHash(Buffer.from(headerHash, "hex")),
  );
  if (Option.isNone(row)) throw new Error(`No retained payload ${headerHash}`);
  return SDK.daAvailabilityCommitmentHash(
    SDK.buildDaAvailabilityCommitment({
      deploymentIdentity: fixture.contracts.hubOracle.policyId,
      headerHash,
      payload: row.value[DaPayloadsDB.Columns.PAYLOAD_CBOR],
      responseGeometry: parameters().response_geometry,
    }),
  );
};

/** Commits the first block after init and retains its DA payload. */
const commitFirstBlock = async (harness: Harness): Promise<string> => {
  const { fixture, lucidService, globals } = harness;
  await advanceEmulatorPastLatestBlockEndTime(fixture);
  vi.useFakeTimers({ toFake: ["Date"] });
  syncClock(harness);
  await submitDepositAndRefreshBarriers({
    fixture,
    lucidService,
    globals,
    lovelace: 12_000_000n,
  });
  const block = await runCommitWorkerUntilSubmitted({
    fixture,
    lucidService,
    latestBlock: await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    ),
  });
  await retainSubmittedHeaderPayload({
    fixture,
    headerHash: block.submittedHeaderHash,
    submittedTxHash: block.submittedTxHash,
  });
  return block.submittedHeaderHash;
};

const cosignerKey = walletFromSeed(EMULATOR_DA_COSIGNER_SEED_PHRASE, {
  network: "Preprod",
}).paymentKey;

/**
 * Builds a pool transaction from a fresh operator wallet view, signs it with
 * the operator and (for owner-quorum steps) the DA cosigner, and waits for it.
 */
const submitPoolTx = async (
  { fixture }: Harness,
  build: () => Promise<Effect.Effect<TxBuilder, unknown>>,
  { quorum }: { readonly quorum: boolean },
) => {
  fixture.operatorLucid.selectWallet.fromSeed(
    fixture.operatorAccount.seedPhrase,
  );
  const tx = await Effect.runPromise(await build());
  const completed = await tx.complete({ localUPLCEval: true });
  const withOperator = completed.sign.withWallet();
  const signed = await (
    quorum ? withOperator.sign.withPrivateKey(cosignerKey) : withOperator
  ).complete();
  await fixture.operatorLucid.awaitTx(await signed.submit());
  fixture.operatorLucid.selectWallet.fromSeed(
    fixture.operatorAccount.seedPhrase,
  );
};

const quorumConfig = async (harness: Harness) => {
  const daParams = await Effect.runPromise(
    fetchDaParamsUtxo(harness.fixture.operatorLucid, harness.fixture.contracts),
  );
  return {
    poolValidator: harness.fixture.contracts.daBondPool,
    parameters: parameters(),
    pool: { utxo: (await readPool(harness)).utxo },
    daParamsUtxo: daParams.utxo,
    signerKeyHashes: daParams.datum.owners,
  };
};

const beginWithdraw = async (harness: Harness) => {
  const validFrom = BigInt(harness.fixture.emulator.now());
  await submitPoolTx(
    harness,
    async () =>
      SDK.buildBeginDaBondPoolWithdrawTxProgram(harness.fixture.operatorLucid, {
        ...(await quorumConfig(harness)),
        withdrawDelayMs: WITHDRAW_DELAY_MS,
        validity: { validFrom, validTo: validFrom + 60_000n },
      }),
    { quorum: true },
  );
  const pool = await readPool(harness);
  if (pool.datum === "Bonded") throw new Error("BeginWithdraw left it Bonded");
  return pool.datum.Withdrawing.unlock_at;
};

describe.sequential(
  "DA bond pool bootstrap through the node attestation path",
  () => {
    it(
      "real init leaves the pool Bonded with one bond of backing, and the first block attests to Attested{commitment_hash}",
      async () => {
        const harness = await bootstrap();
        const P = parameters();

        const initialPool = await readPool(harness);
        expect(initialPool.datum).toBe("Bonded");
        expect(initialPool.utxo.assets.lovelace).toBe(
          P.da_bond_pool_floor_lovelace + P.da_bond_lovelace,
        );
        expect(initialPool.backing).toBeGreaterThanOrEqual(P.da_bond_lovelace);

        const headerHash = await commitFirstBlock(harness);
        expect(await attestationStatus(harness, headerHash)).toBe("Unattested");

        const { results, logs } = await attestRound(harness, headerHash);
        expect(results.map((result) => result.headerHash)).toEqual([
          headerHash,
        ]);
        expect(results[0]?.initTxHash).not.toBeNull();
        expect(poolSkips(logs)).toEqual([]);
        expect(await attestationStatus(harness, headerHash)).toEqual({
          Attested: {
            commitment_hash: await expectedCommitmentHash(harness, headerHash),
          },
        });
        // Apply burned the attestation and only read the pool.
        expect(await attestationUtxos(harness, headerHash)).toEqual([]);
        const poolAfter = await readPool(harness);
        expect(poolAfter.utxo.txHash).toBe(initialPool.utxo.txHash);
        expect(poolAfter.utxo.outputIndex).toBe(initialPool.utxo.outputIndex);
        expect(poolAfter.datum).toBe("Bonded");
      },
      BOOTSTRAP_TIMEOUT_MS,
    );

    it(
      "skips the round with a typed warning, never failing the loop, while the pool is under-backed or withdrawing, then attests once it backs a bond again",
      async () => {
        const harness = await bootstrap();
        const { fixture } = harness;
        const P = parameters();

        // Drain the pool below one bond through the real owner-quorum
        // withdrawal: Begin, wait out the delay, Complete one minimum top-up.
        const unlockAt = await beginWithdraw(harness);
        fixture.emulator.awaitSlot(
          Math.ceil((Number(unlockAt) + 1 - fixture.emulator.now()) / 1_000) +
            1,
        );
        syncClock(harness);
        const drain = P.da_bond_min_top_up_lovelace;
        const destination = await fixture.operatorLucid.wallet().address();
        await submitPoolTx(
          harness,
          async () =>
            SDK.buildCompleteDaBondPoolWithdrawTxProgram(
              fixture.operatorLucid,
              {
                ...(await quorumConfig(harness)),
                amount: drain,
                destination,
                validity: { validFrom: BigInt(fixture.emulator.now()) },
              },
            ),
          { quorum: true },
        );
        const drained = await readPool(harness);
        expect(drained.datum).toBe("Bonded");
        expect(drained.backing).toBe(P.da_bond_lovelace - drain);

        const headerHash = await commitFirstBlock(harness);

        // Under-backed: skipped, nothing initialised, the header stays open.
        const underBacked = await attestRound(harness, headerHash);
        expect(underBacked.results).toEqual([]);
        const underBackedSkips = poolSkips(underBacked.logs);
        expect(underBackedSkips).toHaveLength(1);
        expect(underBackedSkips[0]).toMatchObject({
          level: "WARN",
          annotations: {
            event: DA_ATTESTATION_POOL_SKIP_EVENT,
            reason: "pool-under-backed",
            headerHash,
          },
        });
        expect(await attestationUtxos(harness, headerHash)).toEqual([]);
        expect(await attestationStatus(harness, headerHash)).toBe("Unattested");

        // Topped back to one bond, then withdrawing: skipped again.
        await submitPoolTx(
          harness,
          async () =>
            SDK.buildTopUpDaBondPoolTxProgram(fixture.operatorLucid, {
              poolValidator: fixture.contracts.daBondPool,
              parameters: P,
              pool: { utxo: (await readPool(harness)).utxo },
              amount: drain,
            }),
          { quorum: false },
        );
        expect((await readPool(harness)).backing).toBe(P.da_bond_lovelace);
        await beginWithdraw(harness);
        const withdrawing = await attestRound(harness, headerHash);
        expect(withdrawing.results).toEqual([]);
        const withdrawingSkips = poolSkips(withdrawing.logs);
        expect(withdrawingSkips).toHaveLength(1);
        expect(withdrawingSkips[0]).toMatchObject({
          level: "WARN",
          annotations: {
            event: DA_ATTESTATION_POOL_SKIP_EVENT,
            reason: "pool-withdrawing",
            headerHash,
          },
        });
        expect(await attestationUtxos(harness, headerHash)).toEqual([]);
        expect(await attestationStatus(harness, headerHash)).toBe("Unattested");

        // Cancelled: the next round attests the same header.
        await submitPoolTx(
          harness,
          async () =>
            SDK.buildCancelDaBondPoolWithdrawTxProgram(
              fixture.operatorLucid,
              await quorumConfig(harness),
            ),
          { quorum: true },
        );
        const recovered = await attestRound(harness, headerHash);
        expect(recovered.results.map((result) => result.headerHash)).toEqual([
          headerHash,
        ]);
        expect(poolSkips(recovered.logs)).toEqual([]);
        expect(await attestationStatus(harness, headerHash)).toEqual({
          Attested: {
            commitment_hash: await expectedCommitmentHash(harness, headerHash),
          },
        });
      },
      BOOTSTRAP_TIMEOUT_MS,
    );
  },
);
