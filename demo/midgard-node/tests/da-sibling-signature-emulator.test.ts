/**
 * C4 (#792): a committee member who signs two sibling headers is not
 * slashable for it.
 *
 * One member signs the availability commitments of two sibling headers, A and
 * B: same parent, different header hashes, as on two L1 forks. On this chain
 * A landed in the state queue and B did not. The member's signatures commit
 * to content (deployment, header hash, payload framing), never to a chain
 * position, and every DA path on chain is keyed by the header hash of a node
 * already in the queue. So:
 *
 * - honest polarity: no slash path is constructible from the B signature.
 *   Each attempt is refused by the validator that owns it, beside a control
 *   that lands, and A still merges with the pooled bond untouched;
 * - adversarial polarity, on the same harness: the real slash path, an
 *   unanswered availability challenge on a block that did land, still takes
 *   one DA bond from the pool.
 *
 * The finding this pins is docs/midgard/decisions/da-sibling-signatures-not-slashable.md.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  advanceToMaturity,
  buildEmptyBlockMerge,
} from "./availability-challenge-pool-slash-lifecycle.build-empty-block-merge.js";
import {
  backingOf,
  expireAndSettle,
  nodeOf,
  snapshot,
  submitBuilt,
  timeoutParams,
} from "./availability-challenge-pool-slash-lifecycle.submit-built.js";
import {
  assertAvailabilityRefusal,
  AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE,
  availabilityDeployment,
  type AvailabilityFixture,
  createAvailabilityFixture,
  openAvailability,
} from "./helpers/availability-challenge-emulator.js";

/** The committee index of the member who signs both siblings. */
const MEMBER = 0;

const signatureHex = (key: CML.PrivateKey, message: Uint8Array) =>
  Buffer.from(key.sign(message).to_raw_bytes()).toString("hex");

const verifies = (key: CML.PrivateKey, message: Uint8Array, hex: string) =>
  key
    .to_public()
    .verify(
      message,
      CML.Ed25519Signature.from_raw_bytes(Buffer.from(hex, "hex")),
    );

/**
 * Sibling B of the fixture's landed block A: the same parent, a different
 * UTxO root and payload, so a different header hash and commitment.
 */
const siblingOf = async (f: AvailabilityFixture) => {
  const header: SDK.StateQueueNode["header"] = {
    ...f.target.stateQueueNode.header,
    utxosRoot: "03".repeat(32),
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const commitment = SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: f.contracts.hubOracle.policyId,
    headerHash,
    payload: Uint8Array.of(0xb0),
    responseGeometry: SDK.availabilityResponseGeometry({
      chunkByteLength: Number(f.parameters.response_geometry.chunk_byte_length),
      trancheByteLength: Number(
        f.parameters.response_geometry.tranche_byte_length,
      ),
      maxTrancheCount: Number(f.parameters.response_geometry.max_tranche_count),
    }),
  });
  return {
    header,
    headerHash,
    commitment,
    message: SDK.daAvailabilityAttestationMessage(commitment),
  };
};

/**
 * The member signs both siblings; A is then attested through Init, threshold
 * signatures and Apply against the pooled bond. With `refuseSiblingPaths`,
 * the two attestation-side uses of the B signature are attempted first and
 * must be refused on chain beside their landing controls.
 */
const attestLandedSiblingA = async (
  f: AvailabilityFixture,
  options: { refuseSiblingPaths: boolean },
) => {
  const { lucid, contracts } = f;
  const sibling = await siblingOf(f);
  const landedHeader = f.target.stateQueueNode.header;
  expect(sibling.header.prevHeaderHash).toBe(landedHeader.prevHeaderHash);
  expect(sibling.headerHash).not.toBe(f.target.headerHash);

  const member = f.committeeKeys[MEMBER]!;
  const messageA = SDK.daAvailabilityAttestationMessage(f.commitment);
  const memberOnA = signatureHex(member, messageA);
  const memberOnB = signatureHex(member, sibling.message);
  // Two valid signatures by one member over sibling headers.
  expect(verifies(member, messageA, memberOnA)).toBe(true);
  expect(verifies(member, sibling.message, memberOnB)).toBe(true);
  expect(verifies(member, messageA, memberOnB)).toBe(false);

  lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
  const rescueBeneficiary = await Effect.runPromise(
    SDK.addressDataFromBech32(f.responder.address),
  );
  const init = (
    target: SDK.DaAttestationStateQueueTarget,
    availabilityCommitment: SDK.DaAvailabilityCommitment,
  ) =>
    Effect.runPromise(
      SDK.incompleteInitDaAttestationTxProgram(lucid, contracts, {
        daParamsUtxo: f.daParamsUtxo,
        daParamsDatum: f.daParamsDatum,
        target,
        referenceScripts: f.daReferences,
        attestationOutputLovelace: AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE,
        rescueBeneficiary,
        availabilityCommitment,
      }),
    );
  if (options.refuseSiblingPaths) {
    // B has no queue node, so an attestation for B can only name A's node.
    // Init derives the header from the referenced node and refuses B's.
    await assertAvailabilityRefusal(
      (
        await init(
          { ...f.target, headerHash: sibling.headerHash },
          sibling.commitment,
        )
      ).complete({ coinSelection: true, localUPLCEval: true }),
      { purpose: "mint", script: "da-attestation minting" },
      f.scriptNames,
    );
  }
  // Control: the landed block's Init lands.
  await f.submit(
    "attestation init for A",
    await init(f.target, f.commitment),
    true,
  );

  const attestationUnit = SDK.daAttestationUnit(
    contracts.daAttestation,
    f.target.headerHash,
  );
  const getAttestation = async (): Promise<SDK.DaAttestationUtxo> => {
    const [utxo] = await lucid.utxosAtWithUnit(
      contracts.daAttestation.spendingScriptAddress,
      attestationUnit,
    );
    if (!utxo?.datum) throw new Error("Missing attestation");
    return { utxo, datum: Data.from(utxo.datum, SDK.DaAttestationDatum) };
  };
  const addSignatures = async (memberSignatureHex: string) =>
    Effect.runPromise(
      SDK.incompleteAddDaAttestationSignaturesTxProgram(lucid, contracts, {
        daParamsUtxo: f.daParamsUtxo,
        daParamsDatum: f.daParamsDatum,
        attestation: await getAttestation(),
        witnesses: f.committeeKeys.map((key, signerIndex) => ({
          signerIndex,
          signatureHex:
            signerIndex === MEMBER
              ? memberSignatureHex
              : signatureHex(key, messageA),
        })),
        referenceScripts: f.daReferences,
      }),
    );
  if (options.refuseSiblingPaths)
    // The B signature cannot count toward A's quorum: AddSignatures checks
    // every signature against A's own message.
    await assertAvailabilityRefusal(
      (await addSignatures(memberOnB)).complete({
        coinSelection: true,
        localUPLCEval: true,
      }),
      { purpose: "spend", script: "da-attestation spending" },
      f.scriptNames,
    );
  // Control: the same member's A signature counts; signing B disqualified
  // nothing.
  await f.submit(
    "attestation threshold signatures for A",
    await addSignatures(memberOnA),
    true,
  );

  const apply = await Effect.runPromise(
    SDK.incompleteApplyDaAttestationToStateQueueTxProgram(lucid, contracts, {
      daParamsUtxo: f.daParamsUtxo,
      daParamsDatum: f.daParamsDatum,
      attestation: await getAttestation(),
      target: f.target,
      referenceScripts: f.daReferences,
      availabilityParameters: f.parameters,
      validityRange: {
        validFrom: BigInt(f.emulator.now()),
        validTo: BigInt(f.emulator.now() + 60_000),
      },
    }),
  );
  await f.submit("attestation apply for A", apply, true);
  const [queue] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    f.queueUnit,
  );
  if (!queue) throw new Error("Apply omitted the queue node");
  expect(nodeOf(queue).da_attestation).toEqual({
    Attested: {
      commitment_hash: SDK.daAvailabilityCommitmentHash(f.commitment),
    },
  });
  return { queue, sibling };
};

const siblingQueueNodes = (f: AvailabilityFixture, headerHash: string) =>
  f.lucid.utxosAtWithUnit(
    f.contracts.stateQueue.spendingScriptAddress,
    f.contracts.stateQueue.policyId +
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      headerHash,
  );

const daParamsNow = async (f: AvailabilityFixture): Promise<UTxO> => {
  const [params] = await f.lucid.utxosAtWithUnit(
    f.contracts.daParamsGovernor.spendingScriptAddress,
    f.contracts.daParamsGovernor.policyId + SDK.DA_PARAMS_ASSET_NAME,
  );
  if (!params) throw new Error("Missing DA params");
  return params;
};

describe("a committee member signing sibling headers", () => {
  it("gives no slash path: every use of the sibling signature is refused and the landed block merges", async () => {
    const f = await createAvailabilityFixture(1);
    const poolBefore = await f.getPool();
    const { queue, sibling } = await attestLandedSiblingA(f, {
      refuseSiblingPaths: true,
    });

    // The only node a challenge can open against is A's. Recording B's
    // commitment there is refused by the open yield: the node is
    // `Attested{hash(A)}` under header A, not B.
    const open = await openAvailability(f, {
      queue,
      commitment: f.commitment,
    });
    await assertAvailabilityRefusal(
      open.build({ commitment: sibling.commitment }),
      {
        purpose: "withdraw",
        script: "availability-challenge open withdrawal",
        // The Open's only withdrawal.
        index: 0,
      },
      f.scriptNames,
    );
    // Control: the same Open with A's commitment evaluates (not submitted, so
    // A is never challenged here).
    await open.build().complete({ coinSelection: false, localUPLCEval: true });

    // B never entered the queue, so no challenge, timeout or pool Slash can
    // name it.
    expect(await siblingQueueNodes(f, sibling.headerHash)).toHaveLength(0);

    advanceToMaturity(f, queue);
    const merge = await buildEmptyBlockMerge(f, queue);
    await f.submit("merge the landed sibling A", merge.tx);
    expect(
      await f.lucid.utxosAtWithUnit(
        f.contracts.stateQueue.spendingScriptAddress,
        f.queueUnit,
      ),
    ).toHaveLength(0);
    // The pooled bond is untouched, and the member's committee seat is too:
    // the governed DA params are unchanged.
    expect((await f.getPool()).assets).toEqual(poolBefore.assets);
    const params = await daParamsNow(f);
    expect(params.datum).toBe(f.daParamsUtxo.datum);
    expect(
      Data.from(params.datum!, SDK.DaParamsDatum).committee.slice(
        MEMBER * 64,
        MEMBER * 64 + 64,
      ),
    ).toBe(
      Buffer.from(f.committeeKeys[MEMBER]!.to_public().to_raw_bytes()).toString(
        "hex",
      ),
    );
  }, 240_000);

  it("still slashes the pool when the landed sibling's data is withheld", async () => {
    const f = await createAvailabilityFixture(1);
    const d = availabilityDeployment(f);
    const { queue, sibling } = await attestLandedSiblingA(f, {
      refuseSiblingPaths: false,
    });
    await openAvailability(f, { queue, commitment: f.commitment }).then(
      (open) => open.submit(),
    );
    const s = await snapshot(f, d);
    // Nobody publishes A's bytes before the response deadline.
    const terminal = await expireAndSettle(f, d, s);
    const pool = await f.getPool();
    expect(backingOf(pool)).toBeGreaterThanOrEqual(
      f.parameters.da_bond_lovelace,
    );
    const slash = SDK.planDaBondPoolSlash({
      poolLovelace: pool.assets.lovelace,
      parameters: f.parameters,
    });
    expect(slash.poolOutputLovelace).toBe(
      pool.assets.lovelace - f.parameters.da_bond_lovelace,
    );
    const built = await Effect.runPromise(
      SDK.buildTimeoutDaAvailabilityChallengeTxProgram(
        f.lucid,
        d,
        await timeoutParams(f, s, terminal, pool),
      ),
    );
    await submitBuilt(f, built);
    expect((await f.getPool()).assets.lovelace).toBe(slash.poolOutputLovelace);
    expect((await snapshot(f, d)).queue).toBeUndefined();
    expect(await siblingQueueNodes(f, sibling.headerHash)).toHaveLength(0);
  }, 240_000);
});
