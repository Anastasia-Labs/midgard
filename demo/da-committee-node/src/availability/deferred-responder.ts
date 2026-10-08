import type { CommitteeL1Source } from "../l1/follower/l1-follower.js";
import {
  AvailabilityResponderAwaitingScanError,
  type AvailabilityResponderReport,
} from "./responder.js";

type BuiltResponder = Readonly<{
  responder: Readonly<{ drain: () => Promise<AvailabilityResponderReport> }>;
  close: () => void;
}>;

/**
 * The availability responder, built on the first drain the committee's L1
 * follower is ready for. Its construction reads the follower's facts
 * (reference scripts, the hub oracle, collateral), so it waits for them
 * rather than failing the node's startup: until then every drain reports
 * `awaiting_scan` with the follower's reasons, and a failed construction is
 * reported as a `failed` drain and retried on the next one. The process
 * stays up throughout; `/readyz` names the follower's reasons.
 */
export const deferredAvailabilityResponder = (
  source: Pick<CommitteeL1Source, "readiness">,
  build: () => Promise<BuiltResponder>,
): BuiltResponder => {
  let built: BuiltResponder | undefined;
  let building: Promise<BuiltResponder> | undefined;
  let closed = false;
  const drain = async (): Promise<AvailabilityResponderReport> => {
    if (built === undefined) {
      const held = source.readiness();
      if (held.length > 0)
        return {
          challenges: 0,
          status: "awaiting_scan",
          detail: new AvailabilityResponderAwaitingScanError(
            held.map(({ reason, detail }) => `${reason}: ${detail}`).join("; "),
          ).message,
        };
      building ??= build();
      try {
        const next = await building;
        if (closed) next.close();
        else built = next;
      } catch (error) {
        return {
          challenges: 0,
          status: "failed",
          detail: `Availability responder construction failed: ${error instanceof Error ? error.message : String(error)}`,
        };
      } finally {
        building = undefined;
      }
      if (built === undefined) return { challenges: 0, status: "idle" };
    }
    return built.responder.drain();
  };
  return {
    responder: { drain },
    close: () => {
      closed = true;
      built?.close();
    },
  };
};
