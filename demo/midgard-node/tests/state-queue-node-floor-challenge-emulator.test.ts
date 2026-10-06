import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import {
  CML,
  credentialToAddress,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { availabilityParametersFromExplicitEnvironment } from "../src/services/index.js";
import { ensureAvailabilityChallengeRewardAccountsRegisteredProgram } from "../src/transactions/availability-challenge-registration.js";
import { attestStateQueueOnceProgram } from "../src/transactions/da-attestation.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  DaPayloadsDB,
  Effect,
  type EmulatorFixture,
  fetchLatestCommittedBlock,
  initializeNodeRuntime,
  initializeProtocol,
  makeFixture,
  makeGlobalsService,
  makeLucidRuntimeService,
  Option,
  readKeyHash,
  resetActiveRuntimePaths,
  retainSubmittedHeaderPayload,
  runCommitWorkerUntilSubmitted,
  runNodeCommandProgram,
  runNodeDatabaseEffect,
  SDK,
  submitDepositAndRefreshBarriers,
} from "./deposit-flow-emulator-shared.js";

/**
 * Ruling P3 (spec #685, owed here by #693): a state-queue node holds at least
 * the node floor from its commit until it leaves the queue, and the DA status
 * transitions carry its value exactly. So a node committed at exactly the
 * floor must stay openable: Apply (Attested), Open (Challenged, the largest
 * status) and Close (Published) all land on the ledger with the node still
 * holding exactly the floor. This runs the real node commit worker, the real
 * node attestation round and the production SDK challenge builders against
 * one emulator deployment.
 *
 * The floor is `state_queue_node_min_lovelace_v1` in
 * onchain/aiken/lib/midgard/state-queue.ak, mirrored off chain by
 * `SDK.STATE_QUEUE_NODE_MIN_LOVELACE`. The node's status datum does not
 * grow with the commitment's tranche count (Challenged carries two 32-byte
 * hashes), so the widest challenge record is measured by the SDK record
 * min-UTxO codec test, not here.
 */

const TIMEOUT_MS = 600_000;
const CHALLENGE_WINDOW_MS = BigInt(
  DEPLOYMENT_PROFILES["preprod-testing"].timing.da_challenge_window_ms,
);

const AVAILABILITY_ROLES = [
  "availability-challenge spending",
  "availability-challenge minting",
  "availability-challenge open withdrawal",
  "availability-challenge settle withdrawal",
  "availability-challenge close withdrawal",
  "availability-challenge timeout withdrawal",
  "state-queue spending",
  "state-queue minting",
  "state-queue unavailable-timeout withdrawal",
  "correction-lock spending",
  "da-bond-pool spending",
] as const;

const syncClock = (fixture: EmulatorFixture) =>
  vi.setSystemTime(new Date(fixture.emulator.now()));

/** The production challenge deployment over the fixture's published roles. */
const challengeDeployment = async (
  fixture: EmulatorFixture,
): Promise<SDK.DaAvailabilityDeployment> => {
  const authPolicyId = fixture.contracts.referenceScriptAuth.policyId;
  const referenceScripts: Record<string, UTxO> = {};
  for (const role of AVAILABILITY_ROLES) {
    referenceScripts[role] = await fixture.operatorLucid.utxoByUnit(
      SDK.referenceScriptAuthUnit(authPolicyId, role),
    );
  }
  return {
    contracts: fixture.contracts,
    hubOraclePolicyId: fixture.contracts.hubOracle.policyId,
    referenceScriptAuthPolicyId: authPolicyId,
    parameters: availabilityParametersFromExplicitEnvironment(),
    referenceScripts,
    hubOracleRefInput: await fixture.operatorLucid.utxoByUnit(
      fixture.contracts.hubOracle.policyId + SDK.HUB_ORACLE_ASSET_NAME,
    ),
  };
};

/** The queue node for `headerHash`: its UTxO and its DA status. */
const queueNode = async (
  lucid: LucidEvolution,
  d: SDK.DaAvailabilityDeployment,
  headerHash: string,
) => {
  const snapshot = await SDK.fetchDaAvailabilityChallengeSnapshot(
    lucid,
    d,
    headerHash,
  );
  if (snapshot.queue === undefined)
    throw new Error(`Missing queue node ${headerHash}`);
  const node = await Effect.runPromise(
    SDK.getStateQueueNodeFromStateQueueDatum(snapshot.queue.datum),
  );
  return { snapshot, utxo: snapshot.queue.utxo, status: node.da_attestation };
};

/** The node holds exactly the floor beside its own NFT, nothing else. */
const expectNodeAtFloor = (utxo: UTxO, d: SDK.DaAvailabilityDeployment) => {
  expect(utxo.assets.lovelace).toBe(SDK.STATE_QUEUE_NODE_MIN_LOVELACE);
  const tokens = Object.entries(utxo.assets).filter(
    ([unit]) => unit !== "lovelace",
  );
  expect(tokens).toHaveLength(1);
  expect(tokens[0]![0].startsWith(d.contracts.stateQueue.policyId)).toBe(true);
  expect(tokens[0]![1]).toBe(1n);
};

/**
 * Plain-ADA coins at the wallet's address, read from the ledger rather than
 * the wallet's cached view, largest first, never the excluded out-refs.
 */
const collateralCoins = async (
  lucid: LucidEvolution,
  exclude: readonly UTxO[] = [],
) =>
  (await lucid.utxosAt(await lucid.wallet().address()))
    .filter(
      (u) =>
        Object.keys(u.assets).length === 1 &&
        !exclude.some(
          (e) => e.txHash === u.txHash && e.outputIndex === u.outputIndex,
        ),
    )
    .sort((a, b) => (b.assets.lovelace > a.assets.lovelace ? 1 : -1));

const resources = async (
  fixture: EmulatorFixture,
  lucid: LucidEvolution,
  feeLovelace: bigint,
  exclude: readonly UTxO[] = [],
  responseDeadline?: bigint,
): Promise<SDK.DaAvailabilityTransactionResources> => {
  syncClock(fixture);
  const validFrom = BigInt(fixture.emulator.now());
  const validTo =
    responseDeadline !== undefined &&
    responseDeadline + 1n < validFrom + 60_000n
      ? responseDeadline + 1n
      : validFrom + 60_000n;
  return {
    collateralInputs: await collateralCoins(lucid, exclude),
    feeLovelace,
    validFrom,
    validTo,
  };
};

/** Signs with the builder's wallet, submits and returns the outputs. */
const submitBuilt = async (
  fixture: EmulatorFixture,
  lucid: LucidEvolution,
  built: SDK.BuiltDaAvailabilityTransaction,
) => {
  const signed = await built.tx.sign.withWallet().complete();
  expect(signed.toCBOR().length / 2).toBeLessThanOrEqual(16_384);
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  syncClock(fixture);
  const outputs = CML.Transaction.from_cbor_hex(signed.toCBOR())
    .body()
    .outputs()
    .len();
  return lucid.utxosByOutRef(
    Array.from({ length: outputs }, (_, outputIndex) => ({
      txHash,
      outputIndex,
    })),
  );
};

describe(
  "state-queue node floor through a DA challenge",
  { concurrent: false },
  () => {
    it(
      "a node committed at exactly the floor is attested, opened against and closed with its lovelace unchanged",
      async () => {
        await resetActiveRuntimePaths();
        await initializeNodeRuntime();
        const fixture = await makeFixture();
        await initializeProtocol(fixture);
        const lucidService = await makeLucidRuntimeService(fixture);
        const globals = await makeGlobalsService();
        const { operatorLucid, depositorLucid } = fixture;

        // Commit the first block through the real commit worker.
        await advanceEmulatorPastLatestBlockEndTime(fixture);
        vi.useFakeTimers({ toFake: ["Date"] });
        syncClock(fixture);
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
            operatorLucid,
            fixture.contracts,
          ),
        });
        await retainSubmittedHeaderPayload({
          fixture,
          headerHash: block.submittedHeaderHash,
          submittedTxHash: block.submittedTxHash,
        });
        const headerHash = block.submittedHeaderHash;

        operatorLucid.selectWallet.fromSeed(fixture.operatorAccount.seedPhrase);
        await Effect.runPromise(
          ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
            operatorLucid,
            fixture.contracts,
          ),
        );
        const d = await challengeDeployment(fixture);
        const P = d.parameters;

        const committed = await queueNode(operatorLucid, d, headerHash);
        expect(committed.status).toBe("Unattested");
        expectNodeAtFloor(committed.utxo, d);

        // Apply through the node's own attestation round.
        syncClock(fixture);
        const attested = await runNodeCommandProgram(
          attestStateQueueOnceProgram({ headerHash }),
          { fixture, lucidService, globals },
        );
        expect(attested.map((result) => result.headerHash)).toEqual([
          headerHash,
        ]);
        const afterApply = await queueNode(operatorLucid, d, headerHash);
        const row = await runNodeDatabaseEffect(
          DaPayloadsDB.retrieveByHeaderHash(Buffer.from(headerHash, "hex")),
        );
        if (Option.isNone(row)) throw new Error("No retained DA payload");
        const payload = row.value[DaPayloadsDB.Columns.PAYLOAD_CBOR];
        const commitment = SDK.buildDaAvailabilityCommitment({
          deploymentIdentity: d.hubOraclePolicyId,
          headerHash,
          payload,
          responseGeometry: P.response_geometry,
        });
        expect(afterApply.status).toEqual({
          Attested: {
            commitment_hash: SDK.daAvailabilityCommitmentHash(commitment),
          },
        });
        expectNodeAtFloor(afterApply.utxo, d);

        // Open from an exact isolated coin at the challenger's key address.
        const openFunding =
          P.challenger_bond_lovelace +
          P.challenge_record_lovelace +
          P.max_open_fee_lovelace;
        const challenger = await readKeyHash(depositorLucid);
        const network = depositorLucid.config().network;
        if (network === undefined) throw new Error("Missing emulator network");
        const challengerAddress = credentialToAddress(network, {
          type: "Key",
          hash: challenger,
        });
        depositorLucid.selectWallet.fromSeed(
          fixture.depositorAccount.seedPhrase,
        );
        const fundingTx = await depositorLucid
          .newTx()
          .pay.ToAddress(challengerAddress, { lovelace: openFunding })
          .complete();
        const fundingHash = await (
          await fundingTx.sign.withWallet().complete()
        ).submit();
        await depositorLucid.awaitTx(fundingHash);
        const [challengerFunding] = (
          await depositorLucid.utxosByOutRef(
            [0, 1].map((outputIndex) => ({ txHash: fundingHash, outputIndex })),
          )
        ).filter(
          (u) =>
            u.assets.lovelace === openFunding &&
            Object.keys(u.assets).length === 1,
        );
        if (challengerFunding === undefined)
          throw new Error("Missing exact challenger funding coin");
        await submitBuilt(
          fixture,
          depositorLucid,
          await Effect.runPromise(
            SDK.buildOpenDaAvailabilityChallengeTxProgram(depositorLucid, d, {
              ...(await resources(
                fixture,
                depositorLucid,
                P.max_open_fee_lovelace,
                [challengerFunding],
              )),
              commitment,
              queue: afterApply.utxo,
              challengerFunding,
              challenger,
              daChallengeWindowMs: CHALLENGE_WINDOW_MS,
            }),
          ),
        );
        const opened = await queueNode(operatorLucid, d, headerHash);
        const record = opened.snapshot.recordDatum;
        if (record === undefined) throw new Error("Missing challenge record");
        expect(opened.status).toEqual({
          Challenged: {
            commitment_hash: SDK.daAvailabilityCommitmentHash(commitment),
            challenge_asset_name: record.challenge_asset_name,
          },
        });
        expectNodeAtFloor(opened.utxo, d);

        // The operator answers: every chunk, then settlement, then Close.
        const plans = SDK.planDaAvailabilityPublications({
          commitment: record.commitment,
          payload,
          challengeAssetName: record.challenge_asset_name,
        });
        expect(plans).toHaveLength(opened.snapshot.tranches.length);
        let snapshot = opened.snapshot;
        for (const [trancheIndex, plan] of plans.entries()) {
          let thread = snapshot.tranches[trancheIndex]!.utxo;
          let carrier: UTxO | undefined;
          for (const publication of plan.publications) {
            const outputs = await submitBuilt(
              fixture,
              operatorLucid,
              await Effect.runPromise(
                SDK.buildPublishDaAvailabilityChunkTxProgram(operatorLucid, d, {
                  ...(await resources(
                    fixture,
                    operatorLucid,
                    P.max_publication_fee_lovelace,
                    [],
                    record.response_deadline,
                  )),
                  thread,
                  previousCarrier: carrier,
                  publication,
                }),
              ),
            );
            thread = outputs[0]!;
            carrier = outputs[1]!;
          }
          snapshot = await SDK.fetchDaAvailabilityChallengeSnapshot(
            operatorLucid,
            d,
            headerHash,
          );
          await submitBuilt(
            fixture,
            operatorLucid,
            await Effect.runPromise(
              SDK.buildSettleDaAvailabilityTrancheTxProgram(operatorLucid, d, {
                ...(await resources(
                  fixture,
                  operatorLucid,
                  P.max_settlement_fee_lovelace,
                )),
                record: snapshot.record!,
                terminal: snapshot.terminal!,
                thread,
                carrier,
              }),
            ),
          );
          snapshot = await SDK.fetchDaAvailabilityChallengeSnapshot(
            operatorLucid,
            d,
            headerHash,
          );
        }
        const beforeClose = await queueNode(operatorLucid, d, headerHash);
        expectNodeAtFloor(beforeClose.utxo, d);
        await submitBuilt(
          fixture,
          operatorLucid,
          await Effect.runPromise(
            SDK.buildCloseDaAvailabilityChallengeTxProgram(operatorLucid, d, {
              ...(await resources(
                fixture,
                operatorLucid,
                P.max_close_fee_lovelace,
              )),
              record: snapshot.record!,
              terminal: snapshot.terminal!,
              queue: beforeClose.utxo,
            }),
          ),
        );
        const closed = await queueNode(operatorLucid, d, headerHash);
        expect(closed.snapshot.record).toBeUndefined();
        expect(closed.status).toMatchObject({ Published: {} });
        expectNodeAtFloor(closed.utxo, d);
      },
      TIMEOUT_MS,
    );
  },
);
