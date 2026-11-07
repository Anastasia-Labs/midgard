import { Metric } from "effect";

export const txCounter = Metric.counter("tx_count", {
  description: "A counter for tracking submit transactions",
  bigint: true,
  incremental: true,
});

export const submitHandlerLatencyTimer = Metric.timer(
  "submit_handler_latency",
  "Latency of POST /submit handler responses in milliseconds",
);

export const submitBodyReadDurationTimer = Metric.timer(
  "submit_body_read_duration",
  "Duration of POST /submit request body reads in milliseconds",
);

export const submitNormalizeDurationTimer = Metric.timer(
  "submit_normalize_duration",
  "Duration of POST /submit canonical CBOR validation and normalization in milliseconds",
);

export const submitDurableAdmissionDurationTimer = Metric.timer(
  "submit_durable_admission_duration",
  "Duration of POST /submit durable admission writes in milliseconds",
);

export const submitResponseDurationTimer = Metric.timer(
  "submit_response_duration",
  "Duration of POST /submit response construction in milliseconds",
);

export const submitQueueOfferFailureCounter = Metric.counter(
  "submit_queue_offer_failure_count",
  {
    description:
      "Number of POST /submit requests rejected because the queue was full",
    bigint: true,
    incremental: true,
  },
);
