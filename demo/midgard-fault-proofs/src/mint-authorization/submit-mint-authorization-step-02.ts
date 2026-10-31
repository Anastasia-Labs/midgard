/**
 * `mint-authorization` step-02 submitter.
 *
 * Binds the thread to the committed claim and reads the accused policy id
 * off the committed field-5 item. The disputed header rides the redeemer,
 * the event's transition step and event→step leaf are opened from the
 * transition-trace reconstruction, and the committed mint field opens
 * through the §8.8 door on whatever §8 carriage tier its own byte length
 * selects — a small mint rides tier-1 inline, a large one is published as
 * tier-2 RawUtxo and read back, never a forced tier. Every check the validator
 * makes that this process can make locally is made locally first, so a
 * doomed transaction is refused before it costs anything:
 *
 * - the reconstruction's header must hash to the thread NFT's asset-name
 *   tail (blake2b-224 of the serialised header Data);
 * - the accused ordinal must land inside the decoded committed mint field
 *   (the decode also re-asserts §5.6 canonicality — non-canonical committed
 *   bytes are the decoding family's dispute, not this one's);
 * - the direction must be in the family's two-value domain.
 *
 * The policy id in the step-03 state is READ off the committed item — the
 * caller names only the ordinal, exactly like the validator.
 */

import "@al-ft/midgard-core";
import "@al-ft/midgard-core/plutus-data-cbor";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "../field-opening.js";
import "../runtime.js";
import "../spend-input-witness.js";
import "../step-support.js";
import "../tx-layout.js";
import "../witness-reference-scripts.js";
import "../workflow/raw-datum-preimage.js";
import "../workflow/structured-data-preimage.js";
import "../workflow/transaction-boundary.js";
import "./evidence.js";
import "./mint-scan.js";
import "./submit-common.js";
import "./submit-mint-authorization-step-02.submit-mint-authorization-step02-result.js";
import "./submit-mint-authorization-step-02.submit-mint-authorization-step02.js";
export { submitMintAuthorizationStep02 } from "./submit-mint-authorization-step-02.submit-mint-authorization-step02.js";
export { type SubmitMintAuthorizationStep02Result } from "./submit-mint-authorization-step-02.submit-mint-authorization-step02-result.js";
