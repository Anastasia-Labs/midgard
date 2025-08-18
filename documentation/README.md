# Midgard Documentation

This directory contains the documentation website for Midgard, built with [Nextra](https://nextra.site).

## Development

To start the development server:

```
pnpm install
```

```
pnpm dev
```

> Local development server will be available at [http://localhost:3000](http://localhost:3000).

## Structure

```
documentation/
├── app/
├── components/
├── content/          # Documentation content (MDX files)
│   ├── index.mdx     # Landing page
│   ├── user-facing/
│   └── architectural-decision-records/
│       └── 000-example-adr.mdx # ADR template
│
├── public/
└── styles/
```

## Writing Documentation

### Regular Documentation

Add your `.mdx` files to the appropriate directory under `content/`.Documentation content is written in MDX with optional Nextra components and stored in the `content` directory. The sidebar navigation is automatically generated from the file structure.

### Architectural Decisions

For new ADRs, copy the template from `content/architectural-decision-records/000-example-adr.mdx`:

1. Create a new file: `content/architectural-decision-records/00X-title.mdx`
2. Fill in the sections
