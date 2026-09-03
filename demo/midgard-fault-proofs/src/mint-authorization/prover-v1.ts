/**
 * `mint-authorization` prover-side scan.
 *
 * Given one operator-ACCEPTED committed native transaction's field
 * preimages (and the reference-script anchors of its resolved reference
 * inputs), finds every mint policy the family can convict on:
 *
 * - **direction A (script absent)**: the policy id hashes to no script in
 *   the transaction's machine-consulted source surface — the field-6 inline
 *   script witnesses plus the reference scripts of the resolved field-1
 *   reference inputs;
 * - **direction B (script unsatisfied)**: the policy's script is present
 *   and is a native script whose machine-twin evaluation over the
 *   committed field-7 signer set and the committed validity interval yields
 *   `satisfied: False`.
 *
 * A present-but-satisfied policy, and a present Plutus policy, yield no
 * finding — the latter is machine/interactive territory, not this family's.
 * A script-witness field that does not decode is the decoding family's
 * territory and this scan refuses the transaction rather than classifying
 * it.
 *
 * The scan works exclusively off committed bytes: nothing here trusts a
 * mempool or an operator's account of the transaction.
 */
import {
  decodeMidgardAddressWitnessFieldPreimage,
  decodeMidgardMintFieldPreimage,
  decodeMidgardNativeScript,
  decodeMidgardScriptWitnessFieldPreimage,
  encodeMidgardNativeScript,
  hashMidgardVersionedScript,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeScript,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import {
  hashHexWithBlake2b,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { MINT_AUTHORIZATION_CATEGORY_LABEL } from "./contracts-v1.js";

const proverError = (message: string): Error =>
  new Error(`${MINT_AUTHORIZATION_CATEGORY_LABEL} prover: ${message}`);

/**
 * The reference-script anchor of one resolved reference input, in field-1
 * order. `referenceScriptLanguage === -1n` means the resolved output
 * carries no reference script (`referenceScriptHashHex` is then ignored).
 */
export type MintAuthorizationResolvedReferenceScript = {
  readonly referenceScriptLanguage: bigint;
  readonly referenceScriptHashHex: string;
};

/** One convictable mint policy. */
export type MintAuthorizationFinding = {
  /** Ordinal of the policy's item in the committed field 5. */
  readonly policyIndex: bigint;
  readonly policyIdHex: string;
  /** 0 = script absent, 1 = script unsatisfied. */
  readonly direction: bigint;
  /**
   * Direction B only: the unsatisfied native script's canonical payload
   * bytes (the step-03 redeemer's `script_bytes`), hex. `null` for
   * direction A.
   */
  readonly scriptBytesHex: string | null;
};

const keyHashOfVerificationKeyHex = (verificationKeyHex: string): string =>
  Effect.runSync(hashHexWithBlake2b(verificationKeyHex, 28));

/**
 * Scans one committed transaction's mint field. Throws when any of the
 * committed fields fails to decode canonically — a committed field the
 * machine could not have decoded is another family's dispute, never a
 * silent skip.
 */
export const scanMintAuthorization = ({
  mintPreimageCbor,
  scriptTxWitsPreimageCbor,
  addrTxWitsPreimageCbor,
  validityIntervalStart,
  validityIntervalEnd,
  resolvedReferenceScripts,
  nativeScriptBytesByHashHex = {},
}: {
  readonly mintPreimageCbor: Uint8Array;
  readonly scriptTxWitsPreimageCbor: Uint8Array;
  readonly addrTxWitsPreimageCbor: Uint8Array;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  /** One anchor per committed field-1 reference input, in field order. */
  readonly resolvedReferenceScripts: readonly MintAuthorizationResolvedReferenceScript[];
  /**
   * Optional side table for direction B when the policy's native script is
   * sourced through a reference input rather than inline: canonical native
   * payload bytes (hex) keyed by script hash (hex). Entries are verified by
   * re-hash before use, so a wrong table cannot fabricate a finding.
   */
  readonly nativeScriptBytesByHashHex?: Readonly<Record<string, string>>;
}): readonly MintAuthorizationFinding[] => {
  const mintItems = decodeMidgardMintFieldPreimage(
    Buffer.from(mintPreimageCbor),
  );
  const inlineScripts = decodeMidgardScriptWitnessFieldPreimage(
    Buffer.from(scriptTxWitsPreimageCbor),
  );
  const addressWitnesses = decodeMidgardAddressWitnessFieldPreimage(
    Buffer.from(addrTxWitsPreimageCbor),
  );

  const inlineByHash = new Map<string, (typeof inlineScripts)[number]>();
  for (const script of inlineScripts) {
    inlineByHash.set(hashMidgardVersionedScript(script), script);
  }
  const referencedHashes = new Set<string>(
    resolvedReferenceScripts
      .filter((anchor) => anchor.referenceScriptLanguage !== -1n)
      .map((anchor) => anchor.referenceScriptHashHex.toLowerCase()),
  );

  const witnessSigners: ReadonlySet<string> = new Set(
    addressWitnesses.map((witness) =>
      keyHashOfVerificationKeyHex(
        Buffer.from(witness.verificationKey).toString("hex"),
      ),
    ),
  );
  const verifierInput = {
    validityIntervalStart:
      validityIntervalStart === MIDGARD_POSIX_TIME_NONE
        ? undefined
        : validityIntervalStart,
    validityIntervalEnd:
      validityIntervalEnd === MIDGARD_POSIX_TIME_NONE
        ? undefined
        : validityIntervalEnd,
    witnessSigners,
  };

  const evaluateNative = (
    nativeScript: MidgardNativeScript,
  ): { readonly satisfied: boolean; readonly bytesHex: string } => ({
    satisfied: verifyMidgardNativeScript(nativeScript, verifierInput),
    bytesHex: encodeMidgardNativeScript(nativeScript).toString("hex"),
  });

  const findings: MintAuthorizationFinding[] = [];
  mintItems.forEach((item, index) => {
    const policyIdHex = Buffer.from(item.policyId).toString("hex");
    const inline = inlineByHash.get(policyIdHex);
    const referenced = referencedHashes.has(policyIdHex);
    if (inline === undefined && !referenced) {
      findings.push({
        policyIndex: BigInt(index),
        policyIdHex,
        direction: MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT,
        scriptBytesHex: null,
      });
      return;
    }
    // Present: only a native payload is single-party adjudicable here.
    if (inline !== undefined && inline.language === "NativeCardano") {
      const verdict = evaluateNative(inline.nativeScript);
      if (!verdict.satisfied) {
        findings.push({
          policyIndex: BigInt(index),
          policyIdHex,
          direction: MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED,
          scriptBytesHex: verdict.bytesHex,
        });
      }
      return;
    }
    if (inline === undefined && referenced) {
      const suppliedBytesHex = nativeScriptBytesByHashHex[policyIdHex];
      if (suppliedBytesHex === undefined) {
        // Referenced script whose payload the caller did not supply: not
        // classifiable from committed bytes alone, and possibly Plutus —
        // no finding.
        return;
      }
      const supplied = decodeSuppliedNativeScript(suppliedBytesHex);
      const expected = hashMidgardVersionedScript({
        language: "NativeCardano",
        scriptBytes: supplied.cbor,
        nativeScript: supplied.script,
      });
      if (expected !== policyIdHex) {
        throw proverError(
          `supplied native payload for ${policyIdHex} hashes to ${expected}`,
        );
      }
      const verdict = evaluateNative(supplied.script);
      if (!verdict.satisfied) {
        findings.push({
          policyIndex: BigInt(index),
          policyIdHex,
          direction: MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED,
          scriptBytesHex: verdict.bytesHex,
        });
      }
    }
    // Present Plutus policy: the machine's interactive lane, no finding.
  });
  return findings;
};

/**
 * `decodeMidgardNativeScript` enforces canonical form and exact consumption,
 * matching the on-chain hash pin's domain.
 */
const decodeSuppliedNativeScript = (
  bytesHex: string,
): { readonly script: MidgardNativeScript; readonly cbor: Buffer } => {
  if (!/^(?:[0-9a-f]{2})+$/u.test(bytesHex)) {
    throw proverError("supplied native payload must be lowercase hex bytes");
  }
  return decodeMidgardNativeScript(Buffer.from(bytesHex, "hex"));
};
