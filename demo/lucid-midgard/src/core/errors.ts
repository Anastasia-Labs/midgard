export class LucidMidgardError extends Error {
  readonly code: string;
  readonly detail: string | null;

  constructor(
    message: string,
    detail: string | null = null,
    code = "LUCID_MIDGARD_ERROR",
  ) {
    super(message);
    this.name = "LucidMidgardError";
    this.code = code;
    this.detail = detail;
  }

  toJSON(): Record<string, unknown> {
    return {
      name: this.name,
      code: this.code,
      message: this.message,
      detail: this.detail,
    };
  }
}

export class BuilderInvariantError extends LucidMidgardError {
  constructor(message: string, detail: string | null = null) {
    super(message, detail, "BUILDER_INVARIANT");
    this.name = "BuilderInvariantError";
  }
}

export class InsufficientFundsError extends LucidMidgardError {
  readonly unit: string;
  readonly required: bigint;
  readonly available: bigint;
  readonly feeIncluded: boolean;

  constructor({
    unit,
    required,
    available,
    feeIncluded = false,
  }: {
    readonly unit: string;
    readonly required: bigint;
    readonly available: bigint;
    readonly feeIncluded?: boolean;
  }) {
    super(
      `Insufficient ${unit}: required ${required.toString(10)}, available ${available.toString(10)}`,
      null,
      "INSUFFICIENT_FUNDS",
    );
    this.name = "InsufficientFundsError";
    this.unit = unit;
    this.required = required;
    this.available = available;
    this.feeIncluded = feeIncluded;
  }

  override toJSON(): Record<string, unknown> {
    return {
      ...super.toJSON(),
      unit: this.unit,
      required: this.required.toString(10),
      available: this.available.toString(10),
      feeIncluded: this.feeIncluded,
    };
  }
}

export class ProviderError extends LucidMidgardError {
  readonly endpoint: string;
  readonly statusCode?: number;
  readonly retryable: boolean;

  constructor({
    message,
    detail = null,
    code = "PROVIDER_ERROR",
    endpoint,
    statusCode,
    retryable = false,
  }: {
    readonly message: string;
    readonly detail?: string | null;
    readonly code?: string;
    readonly endpoint: string;
    readonly statusCode?: number;
    readonly retryable?: boolean;
  }) {
    super(message, detail, code);
    this.name = "ProviderError";
    this.endpoint = endpoint;
    this.statusCode = statusCode;
    this.retryable = retryable;
  }

  override toJSON(): Record<string, unknown> {
    return {
      ...super.toJSON(),
      endpoint: this.endpoint,
      statusCode: this.statusCode,
      retryable: this.retryable,
    };
  }
}

export class ProviderCapabilityError extends ProviderError {
  constructor(endpoint: string, message: string) {
    super({
      message,
      code: "PROVIDER_CAPABILITY_UNAVAILABLE",
      endpoint,
      retryable: false,
    });
    this.name = "ProviderCapabilityError";
  }
}

export class ProviderPayloadError extends ProviderError {
  constructor(endpoint: string, message: string, detail: string | null = null) {
    super({
      message,
      detail,
      code: "PROVIDER_PAYLOAD_INVALID",
      endpoint,
      retryable: false,
    });
    this.name = "ProviderPayloadError";
  }
}

export class ProviderHttpError extends ProviderError {
  constructor({
    endpoint,
    statusCode,
    message,
    detail = null,
    retryable,
  }: {
    readonly endpoint: string;
    readonly statusCode: number;
    readonly message: string;
    readonly detail?: string | null;
    readonly retryable?: boolean;
  }) {
    super({
      message,
      detail,
      code: "PROVIDER_HTTP_ERROR",
      endpoint,
      statusCode,
      retryable: retryable ?? statusCode >= 500,
    });
    this.name = "ProviderHttpError";
  }
}

export class ProviderTransportError extends ProviderError {
  constructor(endpoint: string, cause: unknown) {
    super({
      message: `Provider transport error at ${endpoint}`,
      detail: cause instanceof Error ? cause.message : String(cause),
      code: "PROVIDER_TRANSPORT_ERROR",
      endpoint,
      retryable: true,
    });
    this.name = "ProviderTransportError";
  }
}

export class SigningError extends LucidMidgardError {
  constructor(message: string, detail: string | null = null) {
    super(message, detail, "SIGNING_ERROR");
    this.name = "SigningError";
  }
}
