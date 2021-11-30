import React from "react";
import { PageHeader } from "antd";
import Navbar from "../Components/NavBar";

type WelcomePageProps = {};

export function ContactPage(props: WelcomePageProps) {
  return (
    <div className="flexColumn">
      <Navbar />
      <div className="center">
        <PageHeader title="Contact us" />
        <p>If you know some best practices on securing an erlang application, please teach us! We are omegab and gdenhez on discord :-D</p>
      </div>
    </div>
  );
}
