import { MerkleRoot, Proof, RootDomainSchema } from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  type WatcherUserEventReferenceEvidence,
  watcherUserEventReferenceOutput,
} from ".././user-event-reference-authority.js";
import { eventSchemas } from "./decode.canonical-datum-for-output.js";
import {
  dataRoundTrip,
  exactlyOneAsset,
  inlineDatumCbor,
  referencedOutRefAt,
} from "./decode.forced-order-material-field-count.js";
import { type DecodedTerminalSpend } from "./decode.scan-created-transaction-events.js";
import { isHex28, isHex32 } from "./policy.js";
import {
  type EventSchema,
  type WatcherIndexedUserEvent,
  type WatcherUserEventTerminalStatus,
} from "./types.js";

export const outputValue = (
  output: CML.TransactionOutput,
): ReadonlyMap<string, bigint> => {
  const result = new Map<string, bigint>([
    ["lovelace", output.amount().coin()],
  ]);
  const multiAsset = output.amount().multi_asset();
  const policies = multiAsset.keys();
  for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
    const policyId = policies.get(policyIndex);
    const assets = multiAsset.get_assets(policyId);
    if (assets === undefined) {
      continue;
    }
    for (let assetIndex = 0; assetIndex < assets.len(); assetIndex += 1) {
      const assetName = assets.keys().get(assetIndex);
      result.set(
        `${policyId.to_hex()}${assetName.to_hex()}`,
        assets.get(assetName) ?? 0n,
      );
    }
  }
  return result;
};

export const expectedTerminalValue = (
  event: WatcherIndexedUserEvent,
  input: CML.TransactionOutput,
  hubDatum: Record<string, unknown>,
  status: WatcherUserEventTerminalStatus,
): ReadonlyMap<string, bigint> => {
  const expected = new Map(outputValue(input));
  expected.delete(`${event.policyId}${event.assetNameHex}`);
  if (status === "payout_initialized" && isHex28(hubDatum.payout)) {
    expected.set(`${hubDatum.payout}${event.assetNameHex}`, 1n);
  }
  return expected;
};

export const sameValue = (
  left: ReadonlyMap<string, bigint>,
  right: ReadonlyMap<string, bigint>,
): boolean =>
  left.size === right.size &&
  [...left].every(([unit, quantity]) => right.get(unit) === quantity);

export const eventKeyValueCbor = (
  event: WatcherIndexedUserEvent,
): Readonly<{ key: string; value: string; payload: unknown }> | null => {
  try {
    const parsed = Data.from(
      event.eventCborHex,
      eventSchemas(event.kind).event,
    ) as { id: unknown; info?: unknown; tx?: unknown };
    const plutus = CML.PlutusData.from_cbor_hex(event.eventCborHex)
      .as_constr_plutus_data()
      ?.fields();
    if (plutus === undefined || plutus.len() !== 2) {
      return null;
    }
    return {
      key: plutus.get(0).to_cbor_hex(),
      value: plutus.get(1).to_cbor_hex(),
      payload: parsed.info ?? parsed.tx,
    };
  } catch {
    return null;
  }
};

export const countedRootMatches = (
  proof: DecodedTerminalSpend["membershipProof"],
  expectedDomain:
    | "DepositsRootDomain"
    | "WithdrawalsRootDomain"
    | "ForcedTransactionsV1RootDomain",
  expectedRoot: unknown,
): boolean => {
  if (
    proof.domain !== expectedDomain ||
    proof.root !== expectedRoot ||
    proof.count <= 0n ||
    !isHex32(proof.root) ||
    !isHex32(proof.phas_root)
  ) {
    return false;
  }
  const tag = Buffer.from("MidgardRootCountV1", "utf8");
  const domain = Buffer.from(
    Data.to(expectedDomain as never, RootDomainSchema as never),
    "hex",
  );
  const count = Buffer.from(
    Data.to(proof.count as never, Data.Integer() as never),
    "hex",
  );
  return (
    Buffer.from(
      blake2b(
        Buffer.concat([
          tag,
          domain,
          Buffer.from(proof.phas_root, "hex"),
          count,
        ]),
        { dkLen: 32 },
      ),
    ).toString("hex") === proof.root
  );
};

export const membershipWithdrawalCbor = (
  proof: DecodedTerminalSpend["membershipProof"],
): string | null => {
  try {
    const values = CML.PlutusDataList.new();
    values.add(
      CML.PlutusData.from_cbor_hex(
        Data.to(proof.phas_root as never, MerkleRoot as never),
      ),
    );
    values.add(CML.PlutusData.new_bytes(Buffer.from(proof.key, "hex")));
    values.add(CML.PlutusData.new_bytes(Buffer.from(proof.value, "hex")));
    values.add(
      CML.PlutusData.from_cbor_hex(
        Data.to(proof.proof as never, Proof as never),
      ),
    );
    return CML.PlutusData.new_list(values).to_canonical_cbor_hex();
  } catch {
    return null;
  }
};

export const authenticReferenceDatum = (
  referenceEvidence: WatcherUserEventReferenceEvidence,
  transactionHash: string,
  body: CML.TransactionBody,
  index: bigint,
  policyId: string,
  schema: EventSchema,
): Readonly<{
  output: CML.TransactionOutput;
  datum: Record<string, unknown>;
}> | null => {
  const output = watcherUserEventReferenceOutput(
    referenceEvidence,
    transactionHash,
    referencedOutRefAt(body, index),
  );
  const datumCbor = output === null ? null : inlineDatumCbor(output);
  if (
    output === null ||
    datumCbor === null ||
    exactlyOneAsset(output, policyId)?.quantity !== 1n
  ) {
    return null;
  }
  const datum = dataRoundTrip<Record<string, unknown>>(datumCbor, schema);
  return datum === null ? null : { output, datum };
};

export const cardanoDatumMatches = (
  output: CML.TransactionOutput,
  expected: unknown,
): boolean => {
  if (expected === "NoDatum") {
    return output.datum() === undefined;
  }
  const candidate = expected as {
    DatumHash?: { hash?: unknown };
    InlineDatum?: { data?: unknown };
  };
  if (candidate.DatumHash !== undefined) {
    return (
      (
        output.datum() as
          | { as_hash?: () => { to_hex(): string } | undefined }
          | undefined
      )
        ?.as_hash?.()
        ?.to_hex() === candidate.DatumHash.hash
    );
  }
  if (candidate.InlineDatum !== undefined) {
    try {
      return (
        output.datum()?.as_datum()?.to_cbor_hex() ===
        Data.to(candidate.InlineDatum.data as never)
      );
    } catch {
      return false;
    }
  }
  return false;
};
