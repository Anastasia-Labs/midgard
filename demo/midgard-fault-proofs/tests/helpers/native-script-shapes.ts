/** Native scripts whose phase-A evaluation reads every kind of token a
 * native script has, over a transaction the owner signs. */
import { nativeScriptWitness } from "@al-ft/midgard-validation/tests/validation-fixtures";

import { extraSignerKeyHash } from "./plutus-execution-block-fixture.js";
import { ownerKeyHash } from "./plutus-execution-seeds.js";

type Sig = Readonly<{ type: "sig"; keyHash: Buffer }>;
const sig = (keyHash: Buffer): Sig => ({ type: "sig", keyHash });

/**
 * An `any` script, satisfied by the owner's signature first, whose other
 * branches the evaluation reads in turn: signatures of a key below every
 * signer, above every signer, and between the owner and the first extra
 * signer (so the transaction carries one extra signer), a time lock, empty
 * `all` and `atLeast 0` containers, and non-empty `atLeast` and `all`
 * containers.
 */
export const everyTokenNativeScript = () => {
  const owner = Buffer.from(ownerKeyHash(), "hex");
  const [low, high] = [owner, Buffer.from(extraSignerKeyHash(0), "hex")].sort(
    Buffer.compare,
  ) as [Buffer, Buffer];
  const between = Buffer.from(low);
  between[27] = between[27]! + 1;
  if (Buffer.compare(between, high) >= 0)
    throw new Error("the signers leave no key hash between them");
  return nativeScriptWitness({
    type: "any",
    scripts: [
      sig(owner),
      sig(Buffer.alloc(28, 0)),
      sig(Buffer.alloc(28, 0xff)),
      sig(between),
      { type: "after", slot: 0n },
      { type: "all", scripts: [] },
      { type: "atLeast", required: 0n, scripts: [] },
      { type: "atLeast", required: 1n, scripts: [sig(owner)] },
      { type: "all", scripts: [sig(owner)] },
    ],
  });
};

/** An `any` script whose first branch the owner's signature satisfies and
 * whose second, an empty `all`, holds whoever signs. */
export const ownerOrAnyoneNativeScript = () =>
  nativeScriptWitness({
    type: "any",
    scripts: [
      sig(Buffer.from(ownerKeyHash(), "hex")),
      { type: "all", scripts: [] },
    ],
  });

/**
 * An `any` script the owner's signature satisfies first, whose `count`
 * other branches are each the signature of the key just above the
 * `index`th of `signers` in order, a key none of them holds: over a
 * transaction those signers sign, the evaluation proves that key between
 * the two signers either side of it at each of those branches.
 */
export const betweenSignersNativeScript = (
  signers: readonly string[],
  index: number,
  count: number,
) => {
  const sorted = signers
    .map((keyHash) => Buffer.from(keyHash, "hex"))
    .sort(Buffer.compare);
  const between = Buffer.from(sorted[index]!);
  if (between[27] === 0xff)
    throw new Error("the signer leaves no key hash just above it");
  between[27] = between[27]! + 1;
  if (Buffer.compare(between, sorted[index + 1]!) >= 0)
    throw new Error("the signers leave no key hash between them");
  return nativeScriptWitness({
    type: "any",
    scripts: [
      sig(Buffer.from(ownerKeyHash(), "hex")),
      ...Array.from({ length: count }, () => sig(between)),
    ],
  });
};

/** An `any` script the owner's signature satisfies first, whose `count`
 * other branches are each the signature of a key nobody holds, the least
 * key hash: 128 of them take the script past one chunk. */
export const ownerAmongManyNativeScript = (count: number) =>
  nativeScriptWitness({
    type: "any",
    scripts: [
      sig(Buffer.from(ownerKeyHash(), "hex")),
      ...Array.from({ length: count }, () => sig(Buffer.alloc(28, 0))),
    ],
  });
