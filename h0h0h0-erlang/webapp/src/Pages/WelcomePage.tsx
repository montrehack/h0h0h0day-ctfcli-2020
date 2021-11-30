import React from "react";
import { PageHeader } from "antd";
import Navbar from "../Components/NavBar";

type WelcomePageProps = {};

// Connect to erlang VM by ssh
// user: patate, password: patate, port: 8989
export function WelcomePage(props: WelcomePageProps) {
  return (
    <div className="flexColumn">
      <Navbar />
      <div className="center justify">
        <PageHeader title="Greeting, stranger!" />
        <p>
          On this website, you can find multiple text utilities to make your
          life easier. <br /> We are working on multiple tools for this website.{" "}
          <br />
          We used an erlang backend to be able to easily debug it if something goes wrong. Erlang has amazing debugging capabilities. You only need to connect to the running erlang VM and you can then view the registered processes, trace calls being made... We made sure to have our debugging tools easily available so this site stays up and running!<br />
          If you are an Erlang developer and would like to contribute, please
          contact us.
        </p>
      </div>
    </div>
  );
}
