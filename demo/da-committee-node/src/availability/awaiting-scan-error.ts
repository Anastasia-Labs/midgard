/**
 * The committee's L1 follower holds the committee unready (catching up,
 * waiting on its node, or a rollback it cannot follow), or its view moved
 * while a boundary was read. Every responder step is refused until the
 * follower is ready again, so this aborts the step like any error; but it is
 * the normal wait for the follower, not a failure, and the tick reports it as
 * `awaiting_scan` instead of throwing it. `/readyz` names the reason.
 */
export class AvailabilityResponderAwaitingScanError extends Error {
  constructor(detail?: string) {
    super(
      `Availability responder awaits the committee's L1 follower before acting${detail === undefined ? "" : `: ${detail}`}`,
    );
    this.name = "AvailabilityResponderAwaitingScanError";
  }
}
