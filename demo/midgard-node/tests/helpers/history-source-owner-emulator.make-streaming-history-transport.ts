import { type HistoryTransportRecording } from "./history-source-owner-emulator.history-transport-slots.js";
import { makeHistoryTransport } from "./history-source-owner-emulator.make-history-transport.js";

/** Existing fixed-roster replay controls remain available to existing callers. */
export const makeRecordedHistoryTransport = (
  recorded: HistoryTransportRecording,
) => makeHistoryTransport(recorded, false);

/** Call only after recorded.observer.flush(). Sealed slots cannot be rewritten;
 * successful later observations must have their actual later emulator slot. */
export const makeStreamingHistoryTransport = (
  recorded: HistoryTransportRecording,
) => {
  const transport = makeHistoryTransport(recorded, true);
  return {
    get points() {
      return transport.points;
    },
    requests: transport.requests,
    options: transport.options,
    indexOf: transport.indexOf,
    appendAccepted: transport.appendAccepted,
    close: transport.close,
  };
};
