import {
  ledgerEd25519SignatureReady,
  verifyLedgerEd25519Signature,
} from "@al-ft/midgard-core";
import { verifyAddressWitness } from "@al-ft/midgard-sdk";
import { beforeAll, describe, expect, it } from "vitest";

import { verifyVKeyWitnessSignatures } from "../src/phase-a.validate-input-sets.js";
import {
  type Ed25519Vector,
  ledgerEd25519Verdict,
  namedEd25519Vectors,
  seededEd25519Corpus,
} from "./ed25519-ledger-vectors.js";

/**
 * Every off-chain Ed25519 verdict is the ledger's. Each vector is checked
 * against the `verifyEd25519Signature` builtin as the cek-builtin-cardano-v1
 * goldens pin it (see ed25519-ledger-vectors.ts for the rules, with line
 * references), through the shared verifier, the Phase A witness scan and the
 * SDK's address-witness check.
 *
 * The CI run checks a fixed slice of the seeded corpus; set
 * MIDGARD_ED25519_CORPUS_SIZE to check more of it.
 */
const CORPUS_SEED = "ed25519-ledger-v1";
const CORPUS_SIZE = Number(process.env.MIDGARD_ED25519_CORPUS_SIZE ?? 2_000);

const phaseAVerdict = (vector: Ed25519Vector): boolean =>
  verifyVKeyWitnessSignatures({
    txId: vector.message,
    vkeyWitnesses: [
      {
        index: 0,
        keyHash: Buffer.alloc(28),
        vkey: vector.vkey,
        signature: vector.signature,
      },
    ],
  }) === null;

const sdkVerdict = (vector: Ed25519Vector): boolean =>
  verifyAddressWitness({
    txId: vector.message.toString("hex"),
    witness: {
      verification_key: vector.vkey.toString("hex"),
      signature: vector.signature.toString("hex"),
    },
  });

const SITES = {
  shared: (vector: Ed25519Vector) =>
    verifyLedgerEd25519Signature(vector.vkey, vector.message, vector.signature),
  phaseA: phaseAVerdict,
  sdk: sdkVerdict,
} as const;

/** Each site's verdicts that differ from the ledger's, by vector label. */
const disagreements = (
  vectors: Iterable<Ed25519Vector>,
): { readonly checked: number; readonly differing: readonly string[] } => {
  const differing: string[] = [];
  let checked = 0;
  for (const vector of vectors) {
    checked += 1;
    const ledger = ledgerEd25519Verdict(vector);
    for (const [site, verdict] of Object.entries(SITES)) {
      if (verdict(vector) !== ledger) {
        differing.push(`${site}: ${vector.label} (ledger ${String(ledger)})`);
      }
    }
  }
  return { checked, differing };
};

const message = Buffer.from(
  "4242424242424242424242424242424242424242424242424242424242424242",
  "hex",
);

describe("Ed25519 verification matches the ledger builtin", () => {
  beforeAll(() => ledgerEd25519SignatureReady());

  it("verifies the named vectors exactly as the ledger does", () => {
    const vectors = namedEd25519Vectors(message);
    expect(disagreements(vectors).differing).toStrictEqual([]);
  });

  it("covers both verdicts among the named vectors", () => {
    const verdicts = namedEd25519Vectors(message).map(ledgerEd25519Verdict);
    expect(verdicts.filter((verdict) => verdict).length).toBeGreaterThan(0);
    expect(verdicts.filter((verdict) => !verdict).length).toBeGreaterThan(400);
  });

  it(
    "verifies the seeded corpus exactly as the ledger does",
    { timeout: 600_000 },
    () => {
      const { checked, differing } = disagreements(
        seededEd25519Corpus(CORPUS_SEED, CORPUS_SIZE),
      );
      expect(checked).toBe(CORPUS_SIZE);
      expect(differing).toStrictEqual([]);
    },
  );
});
