import { CML } from "@lucid-evolution/lucid";

import type { FraudProofRawL1Transaction } from "./raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";

/** Adds to `found` every unit `bundle` holds a non-zero quantity of. */
const addUnits = (
  bundle: CML.MultiAsset | CML.Mint,
  found: Set<string>,
): void => {
  const policies = bundle.keys();
  for (let at = 0; at < policies.len(); at += 1) {
    const policy = policies.get(at);
    const policyHex = policy.to_hex();
    const assets = bundle.get_assets(policy)!;
    const names = assets.keys();
    for (let index = 0; index < names.len(); index += 1) {
      const name = names.get(index);
      if (assets.get(name) !== 0n) found.add(policyHex + name.to_hex());
      name.free();
    }
    names.free();
    assets.free();
    policy.free();
  }
  policies.free();
};

/** Adds the units `output` holds to `found`, and frees it: the outputs a
 * history spends can carry datums of hundreds of kilobytes. */
const addUnitsHeld = (
  output: CML.TransactionOutput,
  found: Set<string>,
): void => {
  const value = output.amount();
  const assets = value.multi_asset();
  try {
    addUnits(assets, found);
  } finally {
    assets.free();
    value.free();
    output.free();
  }
};

/** The units the output `outputCbor` holds. */
export const unitsHeldBy = (outputCbor: string): ReadonlySet<string> => {
  const found = new Set<string>();
  addUnitsHeld(CML.TransactionOutput.from_cbor_hex(outputCbor), found);
  return found;
};

/** The facts derived from admitted transactions' bytes, each by the
 * admitted object. */
const facts: WeakMap<object, unknown>[] = [];

/**
 * `derive` over a transaction's bytes, run once per admitted transaction. A
 * capture holds the whole history of each unit it names and a transaction's
 * bytes never change, so what is derived from them is kept by the admitted
 * object and carried to the object the next admission makes of the same
 * bytes (`carryAdmittedFacts`): a long history is decoded once, not once
 * per capture.
 */
export const admittedFact = <Transaction extends object, Fact>(
  derive: (transaction: Transaction) => Fact,
): ((transaction: Transaction) => Fact) => {
  const known = new WeakMap<object, Fact>();
  facts.push(known);
  return (transaction) => {
    if (known.has(transaction)) return known.get(transaction)!;
    const fact = derive(transaction);
    known.set(transaction, fact);
    return fact;
  };
};

/** `next` is `prior` admitted again in exactly the same bytes: every fact
 * derived from `prior`'s bytes is `next`'s. */
export const carryAdmittedFacts = (prior: object, next: object): void => {
  for (const known of facts)
    if (known.has(prior)) known.set(next, known.get(prior));
};

/** Every unit `transaction` spends, creates or mints. */
export const unitsTouchedBy = admittedFact(
  (transaction: FraudProofRawL1Transaction): ReadonlySet<string> => {
    const found = new Set<string>();
    for (const input of transaction.resolvedInputs) {
      addUnitsHeld(
        CML.TransactionOutput.from_cbor_hex(input.outputCbor),
        found,
      );
    }
    const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
    try {
      const outputs = body.outputs();
      try {
        for (let index = 0; index < outputs.len(); index += 1) {
          addUnitsHeld(outputs.get(index), found);
        }
      } finally {
        outputs.free();
      }
      const mint = body.mint();
      if (mint !== undefined) {
        try {
          addUnits(mint, found);
        } finally {
          mint.free();
        }
      }
    } finally {
      body.free();
    }
    return found;
  },
);
