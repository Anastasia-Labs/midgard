import {
  computeHash28,
  encodeMidgardSpendInputItem,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import { admitCompleteCanonicalReplayPredecessor } from "../../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  reencodeFixturePayload,
} from "../helpers/canonical-block-evidence-fixture.js";
import {
  buildMintAuthorizationSubject,
  directionBNativeScript,
  mintItemCborV1,
} from "./mint-authorization-emulator.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";

export const mintAuthorizationRetainedReferenceFixture = async ({
  now,
  operatorVkey,
  count = 1,
  present = true,
  quantity = 1n,
}: {
  now: number;
  operatorVkey: string;
  count?: number;
  present?: boolean;
  quantity?: bigint;
}) => {
  const target = directionBNativeScript();
  const other = directionBNativeScript(0xce);
  const utxos = Array.from({ length: count }, (_, index) => {
    const txId = Buffer.alloc(32);
    txId.writeUInt32BE(index + 1, 28);
    const key = encodeMidgardSpendInputItem({ txId, outputIndex: 0 });
    const policy = present && index === count - 1 ? target : other;
    const script = encodeMidgardVersionedScript({
      language: "NativeCardano",
      nativeScript: policy.script,
      scriptBytes: Buffer.from(policy.scriptBytesHex, "hex"),
    });
    const value = Buffer.concat([
      Buffer.from("a300581d70" + "aa".repeat(28) + "018200a003", "hex"),
      script,
    ]);
    return { key, value };
  });
  const baseline = await buildCanonicalBlockFixture({
    transactions: [],
    utxos,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    startTime: BigInt(now),
    endTime: BigInt(now + 1000),
  });
  const priorHeader = { ...baseline.header, operatorVkey };
  const priorHash = computeHash28(SDK.encodeHeaderCbor(priorHeader)).toString(
    "hex",
  );
  const priorPayload = {
    ...baseline.payload,
    block_body: {
      ...baseline.payload.block_body,
      header: priorHeader,
      header_hash: priorHash,
    },
  };
  const predecessorFixture = {
    ...baseline,
    header: priorHeader,
    headerHash: priorHash,
    payload: priorPayload,
    payloadEnvelopeCbor: await reencodeFixturePayload(priorPayload),
  };
  const subject = buildMintAuthorizationSubject({
    mintItemCbors: [
      mintItemCborV1({
        policyId: Buffer.from(target.policyIdHex, "hex"),
        assetName: Buffer.alloc(0),
        quantity,
      }),
    ],
    referenceInputItemCbors: utxos.map(({ key }) => key.toString("hex")),
  });
  const current = await buildDecodingBlockFixture({
    operatorVkey,
    startTime: BigInt(now + 1000),
    priorLedgerRoot: priorHeader.utxosRoot,
    subject: { kind: "normal", nativeTx: subject.nativeTx },
  });
  const header = {
    ...current.header,
    prevHeaderHash: priorHash,
    prevUtxosRoot: priorHeader.utxosRoot,
    utxosRoot: priorHeader.utxosRoot,
    blockSlot: 1n,
    endTime: BigInt(now + 61_000),
  };
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const payload = {
    ...current.reconstruction.payload,
    block_body: {
      ...current.reconstruction.payload.block_body,
      header,
      header_hash: headerHash,
      utxos: predecessorFixture.payload.block_body.utxos,
    },
  };
  const provenance = {
    trustClass: "public_or_permissionless_da",
    sourceId: "retained-mint-reference",
    grade: "security",
  } as const;
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(predecessorFixture, {
      header,
      headerHash,
    }),
    payloadEnvelopeCbor: await reencodeFixturePayload(payload),
    daProvenance: provenance,
  });
  const context = {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      value: {
        observation: authenticatedHeaderObservation(predecessorFixture),
        payloadEnvelopeCborHex:
          predecessorFixture.payloadEnvelopeCbor.toString("hex"),
        daProvenance: provenance,
      },
      currentEvidence: evidence,
      minimumConfirmationDepth: 1,
    }),
  };
  return {
    evidence,
    context,
    predecessor: predecessorFixture,
    subject,
    target,
  };
};
