/* eslint-env node */
import { Footer, Layout, Navbar } from "nextra-theme-docs";
import { Head } from "nextra/components";
import { getPageMap } from "nextra/page-map";
import "nextra-theme-docs/style.css";
import "../styles/footer.css";
import Image from "next/image";

export const metadata = {
  metadataBase: new URL("https://midgardprotocol.com/"),
  title: {
    template: "%s - Midgard",
  },
  description: "Midgard L2 - Optimistic Rollup for Cardano",
  applicationName: "Midgard",
  generator: "Next.js",
  appleWebApp: {
    title: "Midgard",
  },
  other: {
    "msapplication-TileImage": "/ms-icon-144x144.png",
    "msapplication-TileColor": "#fff",
  },
  twitter: {
    site: "https://x.com/midgardprotocol",
  },
  title: "Midgard Documentation",
  description: "Documentation for Midgard Layer 2 scaling solution for Cardano",
  icons: {
    icon: "/midgard-icon-green.png",
    shortcut: "/midgard-icon-green.png",
    apple: "/midgard-icon-green.png",
  },
};

export default async function RootLayout({ children }) {
  const navbar = (
    <Navbar
      logo={
        <Image
          src="/midgard-icon-white-and-colour.png"
          alt="Midgard Logo"
          width={150}
          height={150}
        />
      }
      chatLink="https://discord.gg/KVJTX343qb"
      projectLink="https://github.com/Anastasia-Labs/midgard"
    />
  );
  const footer = (
    <Footer className="footer-custom">
      <div className="footer-container">
        <div className="footer-content">
          <div className="footer-section logo-section">
            <Image
              src="/midgard-icon-white-and-colour.png"
              alt="Midgard Logo"
              width={140}
              height={140}
              className="footer-logo"
              priority={false}
            />
          </div>
          <div className="footer-links">
            <div className="footer-section">
              <h3>Quick Links</h3>
              <ul>
                <li>
                  <a
                    href="https://anastasialabs.com/"
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    Anastasia Labs
                  </a>
                </li>
              </ul>
            </div>
            <div className="footer-section">
              <h3>Community</h3>
              <ul>
                <li>
                  <a
                    href="https://discord.gg/KVJTX343qb"
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    Discord ⤴
                  </a>
                </li>
                <li>
                  <a
                    href="https://x.com/midgardprotocol"
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    X ⤴
                  </a>
                </li>
              </ul>
            </div>
          </div>
        </div>
        <div className="footer-bottom">
          <p>
            MIT {new Date().getFullYear()} ©{" "}
            <a
              href="https://github.com/Anastasia-Labs/midgard"
              target="_blank"
              rel="noopener noreferrer"
            >
              Midgard
            </a>
          </p>
        </div>
      </div>
    </Footer>
  );

  const pageMap = await getPageMap();
  return (
    <html lang="en" dir="ltr" suppressHydrationWarning>
      <Head
        faviconGlyph="✦"
        color={{
          hue: 150,
          saturation: { light: 50, dark: 60 },
          lightness: { light: 35, dark: 45 },
        }}
        backgroundColor={{
          dark: "#111111",
          light: "#fafafa",
        }}
      >
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <meta name="theme-color" content="#10B981" />
      </Head>
      <body>
        <Layout
          navbar={navbar}
          footer={footer}
          editLink="Edit this page on GitHub"
          docsRepositoryBase="https://github.com/Anastasia-Labs/midgard/blob/main/docs"
          sidebar={{ defaultMenuCollapseLevel: 1 }}
          pageMap={pageMap}
          darkMode={false}
        >
          {children}
        </Layout>
      </body>
    </html>
  );
}
