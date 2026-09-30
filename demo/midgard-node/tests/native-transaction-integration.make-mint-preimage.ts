import {
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldPreimage,
  encodeMidgardFieldPreimageForField,
  type MidgardMintAsset,
  type MidgardMintPolicyItem,
  midgardRedeemerPurposeFromTag,
  sortMidgardMintItems,
} from "@al-ft/midgard-core/codec";
import { plutusDataToCborHex, txOutRefData } from "@al-ft/midgard-validation";
import { CML, Constr } from "@lucid-evolution/lucid";
import { encode } from "cborg";

import {
  makeMidgardTxOutput,
  protectOutputAddressBytes,
} from "./midgard-output-helpers.js";
import { EMPTY_REDEEMER_DATA } from "./native-transaction-integration.script-witness-item-to-versioned.js";

export const makeScriptOutput = (
  scriptHash: CML.ScriptHash,
  lovelace: bigint,
  opts?: {
    readonly datum?: CML.PlutusData;
    readonly scriptRef?: CML.Script;
  },
): Buffer =>
  Buffer.from(
    makeMidgardTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_script(scriptHash),
      ).to_address(),
      CML.Value.from_coin(lovelace),
      opts?.datum !== undefined
        ? CML.DatumOption.new_datum(opts.datum)
        : undefined,
      opts?.scriptRef,
    ).to_cbor_bytes(),
  );

export const makeDatumHashScriptOutput = (
  scriptHash: CML.ScriptHash,
  lovelace: bigint,
  datumHash: CML.DatumHash,
  scriptRef?: CML.Script,
): Buffer => {
  const output = CML.ConwayFormatTxOut.new(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_script(scriptHash),
    ).to_address(),
    CML.Value.from_coin(lovelace),
  );
  output.set_datum_option(CML.DatumOption.new_hash(datumHash));
  if (scriptRef !== undefined) {
    output.set_script_reference(scriptRef);
  }
  return Buffer.from(
    CML.TransactionOutput.new_conway_format_tx_out(output).to_cbor_bytes(),
  );
};

export const makeProtectedScriptOutput = (
  scriptHash: CML.ScriptHash,
  lovelace: bigint,
  opts?: {
    readonly datum?: CML.PlutusData;
    readonly scriptRef?: CML.Script;
  },
): Buffer =>
  protectOutputAddressBytes(makeScriptOutput(scriptHash, lovelace, opts));

const makeScriptValueOutput = (
  scriptHash: CML.ScriptHash,
  value: CML.Value,
  opts?: {
    readonly datum?: CML.PlutusData;
    readonly scriptRef?: CML.Script;
  },
): Buffer =>
  Buffer.from(
    makeMidgardTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_script(scriptHash),
      ).to_address(),
      value,
      opts?.datum !== undefined
        ? CML.DatumOption.new_datum(opts.datum)
        : undefined,
      opts?.scriptRef,
    ).to_cbor_bytes(),
  );

export const makeProtectedScriptValueOutput = (
  scriptHash: CML.ScriptHash,
  value: CML.Value,
  opts?: {
    readonly datum?: CML.PlutusData;
    readonly scriptRef?: CML.Script;
  },
): Buffer =>
  protectOutputAddressBytes(makeScriptValueOutput(scriptHash, value, opts));

/**
 * The §5.1 preimage of field 8, built through the production encoder.
 *
 * The retired counted scheme concatenated bare four-element arrays here; §5.1
 * wraps every item of every field in a definite byte string, so the array-of-
 * arrays form no longer decodes. The `tag` stays a number because that is what
 * `CML.RedeemerTag` and `MidgardRedeemerTag` hand the call sites — §5.3's
 * purpose-tag table does the translation.
 *
 * Ordering is deliberately *not* imposed: several tests below present duplicate
 * or out-of-order pointers on purpose, and §5.3 leaves field 8 unordered.
 */
export const makeRedeemersPreimageCbor = (
  items: readonly {
    readonly tag: number;
    readonly index: bigint;
    readonly data?: Uint8Array;
    readonly exUnits?: readonly [bigint, bigint];
  }[],
): Buffer =>
  encodeMidgardFieldPreimageForField({
    fieldIndex: 8,
    items: items.map((item) => ({
      purpose: midgardRedeemerPurposeFromTag(item.tag),
      index: item.index,
      redeemerCbor: Buffer.from(item.data ?? EMPTY_REDEEMER_DATA),
      executionUnits: {
        memory: item.exUnits?.[0] ?? 1_000_000_000n,
        steps: item.exUnits?.[1] ?? 1_000_000_000n,
      },
    })),
  });

export const makePlutusDataBytes = (value: unknown): Buffer =>
  Buffer.from(plutusDataToCborHex(value), "hex");

export const makeOutRefDataBytes = (outRef: Buffer): Buffer =>
  makePlutusDataBytes(txOutRefData(outRef.toString("hex")));

export const makePlutusContextProbeRedeemer = (opts: {
  readonly expectedOutputReference: Buffer;
  readonly expectedDatum: bigint;
  readonly expectedSigner: string;
  readonly expectedFirstReference: Buffer;
  readonly expectedSecondReference: Buffer;
  readonly expectedPolicy: string;
  readonly expectedAssetName: Buffer;
  readonly expectedMintQuantity: bigint;
  readonly expectedObserver: string;
}): Buffer =>
  makePlutusDataBytes(
    new Constr(0, [
      txOutRefData(opts.expectedOutputReference.toString("hex")),
      opts.expectedDatum,
      opts.expectedSigner,
      txOutRefData(opts.expectedFirstReference.toString("hex")),
      txOutRefData(opts.expectedSecondReference.toString("hex")),
      opts.expectedPolicy,
      opts.expectedAssetName.toString("hex"),
      opts.expectedMintQuantity,
      opts.expectedObserver,
    ]),
  );

export const makeMidgardContextProbeRedeemer = (opts: {
  readonly expectedSpendScriptHash: string;
  readonly expectedOwnRef: Buffer;
  readonly expectedFirstInput: Buffer;
  readonly expectedSecondInput: Buffer;
  readonly expectedFirstReference: Buffer;
  readonly expectedSecondReference: Buffer;
  readonly expectedFirstOutputScriptHash: string;
  readonly expectedSecondOutputScriptHash: string;
  readonly expectedSigner: string;
  readonly expectedObserver: string;
  readonly expectedPolicy: string;
  readonly expectedAssetName: Buffer;
  readonly expectedMintQuantity: bigint;
  readonly expectedMintRedeemer: unknown;
  readonly expectedObserveRedeemer: unknown;
  readonly expectedReceiveScriptHash: string;
  readonly expectedReceiveRedeemer: unknown;
}): Buffer =>
  makePlutusDataBytes(
    new Constr(0, [
      opts.expectedSpendScriptHash,
      txOutRefData(opts.expectedOwnRef.toString("hex")),
      txOutRefData(opts.expectedFirstInput.toString("hex")),
      txOutRefData(opts.expectedSecondInput.toString("hex")),
      txOutRefData(opts.expectedFirstReference.toString("hex")),
      txOutRefData(opts.expectedSecondReference.toString("hex")),
      opts.expectedFirstOutputScriptHash,
      opts.expectedSecondOutputScriptHash,
      opts.expectedSigner,
      opts.expectedObserver,
      opts.expectedPolicy,
      opts.expectedAssetName.toString("hex"),
      opts.expectedMintQuantity,
      opts.expectedMintRedeemer,
      opts.expectedObserveRedeemer,
      opts.expectedReceiveScriptHash,
      opts.expectedReceiveRedeemer,
    ]),
  );

export const makePlutusIntegerData = (value: bigint): CML.PlutusData =>
  CML.PlutusData.new_integer(CML.BigInteger.from_str(value.toString(10)));

/**
 * The §5.6 preimage of field 5, built through the production encoder.
 *
 * The flat `(policy, asset, quantity)` entries the call sites spell are grouped
 * per policy and sorted into §5.6's canonical key order — length first, then
 * byte-lexicographic — at both levels, because the encoder *enforces* that order
 * rather than imposing it, and the decoder rejects anything else. The retired
 * scheme spelled this field as a raw CBOR map and is no longer a field preimage
 * at all.
 */
export const makeMintPreimage = (
  entries: readonly {
    readonly policyId: Uint8Array;
    readonly assetName: Uint8Array;
    readonly quantity: bigint;
  }[],
): Buffer => {
  const policies = new Map<
    string,
    {
      readonly policyId: Buffer;
      readonly assets: Map<string, MidgardMintAsset>;
    }
  >();
  for (const entry of entries) {
    const policyId = Buffer.from(entry.policyId);
    const policy = policies.get(policyId.toString("hex")) ?? {
      policyId,
      assets: new Map<string, MidgardMintAsset>(),
    };
    const assetName = Buffer.from(entry.assetName);
    policy.assets.set(assetName.toString("hex"), {
      assetName,
      quantity: entry.quantity,
    });
    policies.set(policyId.toString("hex"), policy);
  }

  const items: readonly MidgardMintPolicyItem[] = sortMidgardMintItems(
    [...policies.values()].map((policy) => ({
      policyId: policy.policyId,
      assets: [...policy.assets.values()],
    })),
  );

  return encodeMidgardFieldPreimageForField({ fieldIndex: 5, items });
};

/**
 * The retired empty-mint spelling, `a0`, kept only so the negative test below
 * can prove it is refused. §5.1 gives every one of the nine fields exactly one
 * empty spelling — `80`, which is {@link EMPTY_CBOR_LIST}.
 */
export const RETIRED_MINT_MAP_CBOR = Buffer.from(encode(new Map()));

/**
 * A §5.1-valid field-5 preimage whose single §5.6 item is deliberately
 * malformed.
 *
 * The production item encoder refuses a short policy id and an assetless policy
 * by construction, so the item interior is spelled raw here — that is precisely
 * what the negative tests below assert the *decoder* still catches. The §5.1
 * envelope around it stays production-built, so the tests reach §5.6 rather than
 * stopping at the field-preimage gate.
 */
export const makeMalformedMintPolicyItemPreimage = (
  policyId: Uint8Array,
  assets: ReadonlyMap<Uint8Array, bigint>,
): Buffer =>
  encodeMidgardFieldPreimage([
    Buffer.concat([
      Buffer.from([0x82]),
      encodeMidgardDefiniteBytes(Buffer.from(policyId)),
      Buffer.from(
        encode(
          new Map(
            [...assets].map(([assetName, quantity]) => [
              Buffer.from(assetName),
              quantity,
            ]),
          ),
        ),
      ),
    ]),
  ]);
