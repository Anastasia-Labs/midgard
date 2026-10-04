import { assetsEqual } from "@al-ft/midgard-core/assets";
import JSONBig from "json-bigint";
import { decodeLedgerSnapshotOutput } from "midgard-node/l1-ledger-snapshot";

import {
  acceptanceOutRefKey,
  type AcceptancePayoutProof,
  requireAcceptance,
} from "./acceptance-payout-types.js";

const json = JSONBig({ useNativeBigInt: true, strict: true });

/** Exact acquired-ledger outrefs at one source-owned native boundary, not historical address sums. */
export const verifyAcceptanceCurrentPayouts = (
  rawFrame: string,
  proofs: readonly AcceptancePayoutProof[],
  maxFrameBytes: number,
) => {
  requireAcceptance(
    Number.isSafeInteger(maxFrameBytes) &&
      maxFrameBytes > 0 &&
      Buffer.byteLength(rawFrame, "utf8") <= maxFrameBytes,
    "acquired UTxO response exceeds explicit bound",
  );
  requireAcceptance(
    proofs.length === 4 &&
      new Set(proofs.map((proof) => proof.eventId)).size === 4 &&
      new Set(proofs.map((proof) => acceptanceOutRefKey(proof.beneficiary)))
        .size === 4,
    "expected four distinct withdrawal events and beneficiary outrefs",
  );
  const frame: unknown = json.parse(rawFrame);
  requireAcceptance(
    typeof frame === "object" && frame !== null && !Array.isArray(frame),
    "invalid JSON-RPC response",
  );
  const response = frame as Record<string, unknown>;
  requireAcceptance(
    response.jsonrpc === "2.0" &&
      response.error === undefined &&
      Array.isArray(response.result),
    "acquired UTxO query failed or lacks result",
  );
  requireAcceptance(
    response.result.length === proofs.length,
    "spent, missing, duplicate or extra acquired UTxO result",
  );
  const addresses = new Set(proofs.map((proof) => proof.address));
  const rows = response.result.map((value) =>
    decodeLedgerSnapshotOutput(value, addresses),
  );
  const refs = rows.map(acceptanceOutRefKey);
  requireAcceptance(
    new Set(refs).size === refs.length,
    "duplicate acquired UTxO outref",
  );
  return proofs.map((proof) => {
    const ref = acceptanceOutRefKey(proof.beneficiary);
    const output = rows.find((row) => acceptanceOutRefKey(row) === ref);
    requireAcceptance(
      output !== undefined &&
        output.address === proof.address &&
        assetsEqual(output.assets, proof.assets) &&
        output.datum === proof.datum &&
        output.datumHash === proof.datumHash &&
        !output.hasReferenceScript,
      "current exact beneficiary outref/address/full value/datum mismatch",
    );
    return {
      eventId: proof.eventId,
      outRef: ref,
      address: proof.address,
      assets: Object.fromEntries(
        Object.entries(proof.assets).map(([unit, quantity]) => [
          unit,
          quantity.toString(),
        ]),
      ),
      ...(proof.datum === undefined ? {} : { datum: proof.datum }),
      ...(proof.datumHash === undefined ? {} : { datumHash: proof.datumHash }),
    };
  });
};
