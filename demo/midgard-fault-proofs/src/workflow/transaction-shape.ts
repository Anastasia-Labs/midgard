/**
 * The shape every workflow transaction must have before our key signs it,
 * checked from its bytes alone, with no stored state: it pays only governed
 * contracts of the deployment, our wallet and our proof-materials address;
 * it embeds only executable scripts on the deployment's roster, or our
 * proof-materials lock to spend a proof material; it posts collateral from,
 * and returns collateral to, our wallet only; and a fee our coins pay stays
 * within the protocol's per-transaction ceiling.
 */
import { CML, validatorToScriptHash } from "@lucid-evolution/lucid";

import { proofMaterialsScript } from "../funding/proof-materials-address.js";

/** The deployment's governed contracts, as the signed manifest names them. */
export type WorkflowTransactionRoster = Readonly<{
  /** The spending contracts' addresses: where a workflow output may go. */
  contractAddresses: readonly string[];
  /** Every applied script hash: the scripts a transaction may embed. */
  scriptHashes: readonly string[];
}>;

/** What one signer's transactions are held to. */
export type WorkflowTransactionShape = Readonly<{
  roster: WorkflowTransactionRoster;
  /** Our wallet: the funding address. */
  walletAddress: string;
  proofMaterialsAddress: string;
  paymentKeyHash: string;
  /** The protocol's per-transaction fee ceiling. */
  maximumFeeLovelace: bigint;
}>;

/** The plain values of a transaction the shape is checked on. */
export type WorkflowTransactionParts = Readonly<{
  inputs: readonly string[];
  collateralInputs: readonly string[];
  collateralReturnAddress: string | undefined;
  outputAddresses: readonly string[];
  /** The hash of each script the witness set embeds, repeats kept. */
  embeddedScriptHashes: readonly string[];
  fee: bigint;
}>;

const outRefsOf = (inputs: CML.TransactionInputList | undefined): string[] => {
  if (inputs === undefined) return [];
  try {
    return Array.from({ length: inputs.len() }, (_, index) => {
      const input = inputs.get(index);
      const id = input.transaction_id();
      try {
        return `${id.to_hex()}#${input.index().toString()}`;
      } finally {
        id.free();
        input.free();
      }
    });
  } finally {
    inputs.free();
  }
};

/** `output`'s address in bech32; frees what it reads. */
const addressOf = (output: CML.TransactionOutput): string => {
  const address = output.address();
  try {
    return address.to_bech32();
  } finally {
    address.free();
    output.free();
  }
};

type ScriptList = {
  len(): number;
  get(index: number): ScriptItem;
  free(): void;
};
type ScriptItem = { hash(): CML.ScriptHash; free(): void };

const hashesOf = (scripts: ScriptList | undefined): string[] => {
  if (scripts === undefined) return [];
  try {
    return Array.from({ length: scripts.len() }, (_, index) => {
      const script = scripts.get(index);
      const hash = script.hash();
      try {
        return hash.to_hex();
      } finally {
        hash.free();
        script.free();
      }
    });
  } finally {
    scripts.free();
  }
};

/** Reads the parts the shape is checked on, freeing every wasm object. */
export const workflowTransactionParts = (
  signedOrUnsignedCborHex: string,
): WorkflowTransactionParts => {
  const transaction = CML.Transaction.from_cbor_hex(signedOrUnsignedCborHex);
  const body = transaction.body();
  const witnesses = transaction.witness_set();
  const outputs = body.outputs();
  const collateralReturn = body.collateral_return();
  try {
    return Object.freeze({
      inputs: outRefsOf(body.inputs()),
      collateralInputs: outRefsOf(body.collateral_inputs()),
      collateralReturnAddress:
        collateralReturn === undefined
          ? undefined
          : addressOf(collateralReturn),
      outputAddresses: Array.from({ length: outputs.len() }, (_, index) =>
        addressOf(outputs.get(index)),
      ),
      embeddedScriptHashes: [
        ...hashesOf(witnesses.native_scripts()),
        ...[
          witnesses.plutus_v1_scripts(),
          witnesses.plutus_v2_scripts(),
          witnesses.plutus_v3_scripts(),
        ].flatMap(hashesOf),
      ],
      fee: body.fee(),
    });
  } finally {
    outputs.free();
    witnesses.free();
    body.free();
    transaction.free();
  }
};

/**
 * Refuses a transaction outside `shape`. `addressOf` gives the address of an
 * input of the transaction where known (our wallet's coins at least); an
 * input it cannot place is no coin of ours.
 */
export const assertWorkflowTransactionShape = ({
  parts,
  shape,
  addressOf: inputAddress,
}: {
  readonly parts: WorkflowTransactionParts;
  readonly shape: WorkflowTransactionShape;
  readonly addressOf: (outRef: string) => string | undefined;
}): void => {
  const contracts = new Set(shape.roster.contractAddresses);
  const own = new Set([shape.walletAddress, shape.proofMaterialsAddress]);
  for (const address of parts.outputAddresses)
    if (!own.has(address) && !contracts.has(address))
      throw new Error(
        "workflow transaction output escapes the governed contract roster",
      );
  const lock = validatorToScriptHash(
    proofMaterialsScript(shape.paymentKeyHash),
  );
  const rostered = new Set(shape.roster.scriptHashes);
  let locks = 0;
  for (const [index, hash] of parts.embeddedScriptHashes.entries()) {
    if (hash === lock) locks += 1;
    else if (
      !rostered.has(hash) ||
      parts.embeddedScriptHashes.indexOf(hash) !== index
    )
      throw new Error(
        "workflow transaction embeds an executable script witness off the roster",
      );
  }
  const spendsMaterial = parts.inputs.some(
    (outRef) => inputAddress(outRef) === shape.proofMaterialsAddress,
  );
  if (locks > 1)
    throw new Error(
      "workflow transaction embeds an executable script witness off the roster",
    );
  if (locks === 1 && !spendsMaterial)
    throw new Error(
      "workflow transaction embeds our proof-materials script without spending a proof material",
    );
  if (
    parts.collateralInputs.some(
      (outRef) => inputAddress(outRef) !== shape.walletAddress,
    )
  )
    throw new Error("workflow collateral is not our wallet's");
  if (
    parts.collateralReturnAddress !== undefined &&
    parts.collateralReturnAddress !== shape.walletAddress
  )
    throw new Error("workflow collateral return escapes our wallet");
  // A fee only our coins can pay is bounded; a slash's fee is the slashed
  // bond's, which the protocol's validators fix.
  const paidByUs =
    spendsMaterial ||
    parts.inputs.some((outRef) => inputAddress(outRef) === shape.walletAddress);
  if (paidByUs && parts.fee > shape.maximumFeeLovelace)
    throw new Error("workflow transaction fee is outside the policy bound");
};
