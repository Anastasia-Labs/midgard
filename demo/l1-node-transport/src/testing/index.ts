/**
 * Test tooling of the transport. Never imported by production code.
 */
import type { L1NodeTransport } from "../transport.js";

type Member = keyof L1NodeTransport;

// Every public member, checked both ways against the class: a member added
// to or removed from it fails the typecheck here.
const MEMBERS = {
  readiness: "getter",
  onReadiness: "method",
  whenReady: "method",
  withLedgerState: "method",
  query: "method",
  submit: "method",
  hasTx: "method",
  mempoolSizes: "method",
  openChainSync: "method",
  close: "method",
} as const satisfies Record<Member, "getter" | "method">;

/**
 * A stand-in for `L1NodeTransport` made of the members a test supplies, each
 * type-checked against the class; every other member throws
 * `not stubbed: <member>` when used. A getter supplied as a getter stays
 * live. The class holds private state, so no object is assignable to it
 * structurally: this is the one cast every double shares.
 */
export const fakeL1NodeTransport = (
  overrides: Partial<L1NodeTransport> = {},
): L1NodeTransport => {
  const fake = {};
  for (const [member, kind] of Object.entries(MEMBERS)) {
    const notStubbed = (): never => {
      throw new Error(`not stubbed: ${member}`);
    };
    Object.defineProperty(
      fake,
      member,
      kind === "getter"
        ? { get: notStubbed, configurable: true, enumerable: true }
        : {
            value: notStubbed,
            writable: true,
            configurable: true,
            enumerable: true,
          },
    );
  }
  Object.defineProperties(fake, Object.getOwnPropertyDescriptors(overrides));
  return fake as L1NodeTransport;
};
