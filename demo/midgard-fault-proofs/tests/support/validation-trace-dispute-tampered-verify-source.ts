/**
 * A VerifySource other than the one its builder evaluated, presented to the
 * prover funding permit: the builder's signed bytes with their outputs
 * edited or inputs added and signed again by the prover, carrying the
 * builder's own authority (or one amended) granted over them, or the same
 * bytes with no authority.
 */
import { CML, Lucid, type TxSigned, type UTxO } from "@lucid-evolution/lucid";

import {
  grantValidationDisputeFundingAuthority,
  readValidationDisputeFundingAuthority,
} from "../../src/workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import { parseOwnOutRef } from "../../src/workflow/own-outputs.js";
import {
  bindWorkflowPreflightTransaction,
  workflowPreflightTransaction,
} from "../../src/workflow/transaction-boundary.js";
import type { AdapterWrap } from "./validation-trace-dispute-funded-runner.js";
import { isVerifySource } from "./validation-trace-dispute-funded-verify-source.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

/** Rewrites VerifySource's outputs, given the bond, case and wallet addresses. */
export type VerifySourceTamper = (
  outputs: CML.TransactionOutput[],
  addresses: Readonly<{ bond: string; case: string; wallet: string }>,
) => CML.TransactionOutput[];

type Grant = Parameters<typeof grantValidationDisputeFundingAuthority>[0];

/** The authority's terms as its builder grants them. */
export type VerifySourceGrant = Omit<Grant, "signed" | "opener">;

/** How a presented VerifySource differs from the evaluated one. */
export type VerifySourcePresentation = Readonly<{
  outputs?: VerifySourceTamper;
  /** Inputs spent besides the evaluated ones. */
  inputs?: readonly UTxO[];
  /** The authority granted over the presented bytes. */
  authority?: (grant: VerifySourceGrant) => VerifySourceGrant;
  /** The pending claim the authority is granted over, for the spent one. */
  opener?: (opener: UTxO) => UTxO;
}>;

/**
 * What a tampered VerifySource pays its fee above the evaluated one, taken
 * from its change, so the larger body still meets the minimum fee.
 */
const FEE_BUMP = 200_000n;

/**
 * Builds `signed`'s body again with `edit`'s outputs and `inputs` spent
 * besides its own, its change paying `feeBump` more fee, signed by the
 * prover.
 */
const withOutputs = async (
  scene: Scene,
  signed: TxSigned,
  edit: (outputs: CML.TransactionOutput[]) => CML.TransactionOutput[],
  feeBump: bigint,
  inputs: readonly UTxO[] = [],
): Promise<TxSigned> => {
  const transaction = signed.toTransaction();
  const body = transaction.body();
  const original = body.outputs();
  const kept: CML.TransactionOutput[] = [];
  for (let index = 0; index < original.len(); index += 1)
    kept.push(original.get(index));
  const edited = edit(kept);
  const change = changeIndex(edited, scene.h.proverSigner.address);
  const outputs = CML.TransactionOutputList.new();
  edited.forEach((output, index) =>
    outputs.add(
      index === change
        ? withCoin(output, output.amount().coin() - feeBump)
        : output,
    ),
  );
  const spent = body.inputs();
  for (const input of inputs)
    spent.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(input.txHash),
        BigInt(input.outputIndex),
      ),
    );
  const rebuilt = CML.TransactionBody.new(spent, outputs, body.fee() + feeBump);
  const copy = body as unknown as Record<string, () => unknown>;
  const into = rebuilt as unknown as Record<string, (value: unknown) => void>;
  for (const field of [
    "auxiliary_data_hash",
    "certs",
    "collateral_inputs",
    "collateral_return",
    "current_treasury_value",
    "donation",
    "mint",
    "network_id",
    "proposal_procedures",
    "reference_inputs",
    "required_signers",
    "script_data_hash",
    "total_collateral",
    "ttl",
    "validity_interval_start",
    "voting_procedures",
    "withdrawals",
  ]) {
    const value = copy[field]!.call(body);
    if (value !== undefined) into[`set_${field}`]!.call(rebuilt, value);
  }
  const witnesses = CML.TransactionWitnessSet.from_cbor_hex(
    transaction.witness_set().to_cbor_hex(),
  );
  witnesses.set_vkeywitnesses(CML.VkeywitnessList.new());
  const unsigned = CML.Transaction.new(
    rebuilt,
    witnesses,
    true,
    transaction.auxiliary_data(),
  );
  const lucid = await Lucid(scene.h.emulator, "Custom", {
    slotConfig: scene.h.proverLucid.config().slotConfig,
  });
  scene.h.proverSigner.selectWallet(lucid);
  return await lucid
    .fromTx(unsigned.to_cbor_hex())
    .sign.withWallet()
    .complete();
};

export const withCoin = (output: CML.TransactionOutput, coin: bigint) =>
  CML.TransactionOutput.new(
    output.address(),
    CML.Value.new(coin, output.amount().multi_asset()),
    output.datum() ?? null,
    output.script_ref() ?? null,
  );

export const withAddress = (output: CML.TransactionOutput, address: string) =>
  CML.TransactionOutput.new(
    CML.Address.from_bech32(address),
    output.amount(),
    output.datum() ?? null,
    output.script_ref() ?? null,
  );

const atAddress = (output: CML.TransactionOutput, address: string) =>
  output.address().to_bech32() === address;

/**
 * Presents a VerifySource other than the one the builder evaluated: the
 * builder's own authority, amended as `presentation` says, granted again
 * over the presented bytes (or, for `null`, the same bytes with no
 * authority).
 */
export const presentVerifySource =
  (scene: Scene, presentation: VerifySourcePresentation | null): AdapterWrap =>
  (adapter) => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      if (!isVerifySource(context.action)) return preflight;
      const signed = workflowPreflightTransaction(preflight)!;
      const authority = readValidationDisputeFundingAuthority(signed)!;
      const presented =
        presentation === null
          ? await withOutputs(scene, signed, (outputs) => outputs, 0n)
          : await withOutputs(
              scene,
              signed,
              (outputs) =>
                presentation.outputs?.(outputs, {
                  bond: authority.bondAddress,
                  case: authority.caseAddress,
                  wallet: scene.h.proverSigner.address,
                }) ?? outputs,
              FEE_BUMP,
              presentation.inputs,
            );
      if (presentation !== null) {
        const [opener] = await scene.h.emulator.getUtxosByOutRef([
          parseOwnOutRef(authority.openerOutRef),
        ]);
        const grant: VerifySourceGrant = {
          deploymentFingerprint: authority.deploymentFingerprint,
          headerHash: authority.headerHash,
          bond: {
            address: authority.bondAddress,
            unit: authority.bondUnit,
            lovelace: BigInt(authority.bondLovelace),
            datumCborHex: authority.bondDatumCborHex,
          },
          caseOutput: {
            address: authority.caseAddress,
            unit: authority.caseUnit,
            floorLovelace: BigInt(authority.caseFloorLovelace),
            lovelace: BigInt(authority.caseLovelace),
          },
        };
        grantValidationDisputeFundingAuthority({
          ...(presentation.authority?.(grant) ?? grant),
          signed: presented,
          opener: presentation.opener?.(opener!) ?? opener!,
        });
      }
      return bindWorkflowPreflightTransaction(
        { ...preflight, txHash: presented.toHash() },
        presented,
      );
    },
  });

/** The wallet's change output: the largest one paid back to it. */
export const changeIndex = (
  outputs: CML.TransactionOutput[],
  wallet: string,
) => {
  let found = -1;
  outputs.forEach((output, index) => {
    if (
      atAddress(output, wallet) &&
      (found === -1 || output.amount().coin() > outputs[found]!.amount().coin())
    )
      found = index;
  });
  if (found === -1) throw new Error("VerifySource pays no change");
  return found;
};

/** The bond output VerifySource pays. */
export const bondIndex = (outputs: CML.TransactionOutput[], bond: string) => {
  const index = outputs.findIndex((output) => atAddress(output, bond));
  if (index === -1) throw new Error("VerifySource pays no bond");
  return index;
};

/** The case output VerifySource pays. */
export const caseIndex = (
  outputs: CML.TransactionOutput[],
  caseAddress: string,
) => {
  const index = outputs.findIndex((output) => atAddress(output, caseAddress));
  if (index === -1) throw new Error("VerifySource pays no case");
  return index;
};

/** `output` with the inline datum `datumCborHex`. */
export const withDatum = (
  output: CML.TransactionOutput,
  datumCborHex: string,
) =>
  CML.TransactionOutput.new(
    output.address(),
    output.amount(),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumCborHex)),
    output.script_ref() ?? null,
  );

/** `output`'s inline datum. */
export const inlineDatumOf = (output: CML.TransactionOutput): string => {
  const datum = output.datum()?.as_datum();
  if (datum === undefined) throw new Error("the output has no inline datum");
  return datum.to_cbor_hex();
};
