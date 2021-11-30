import { PageHeader, Input, Button } from "antd";
import React, { useState } from "react";
import Navbar from "../Components/NavBar";
import imgNoel from "../bebe-bonhomme-neige-message.png";
import { post } from "../Utils/Post";

type FindsChristmasWordsProps = {};

export function FindsChristmasWordsPage(props: FindsChristmasWordsProps) {
  const [text, setText] = useState("");
  const [response, setResponse] = useState("");

  async function get_christmas_words() {
  try {
   const response = await post(`/api/words`, { text });
   const responseCast: string[] = response;
   if (responseCast) {
     setResponse(responseCast.join("\n"));
   }
   } catch (e) {
     setResponse("");
   }
  }


  return (
    <div className="flexColumn">
      <Navbar />
      <div className="center">
        <PageHeader title="Find christmas words" />
        <img src={imgNoel} width={300} style={{ borderRadius: "6px" }}></img>
        <p>Enter a text and all the christmas words are going to be found.</p>
      </div>
      <div className="flex">
        <div className="w-50 mg-sm">
          <label>Text:</label>
          <Input.TextArea
            size="large"
            maxLength={500}
            onChange={(event) => setText(event.currentTarget.value)}
          />
          <Button
            type="primary"
            htmlType="submit"
            className="w-20"
            style={{ marginTop: "10px" }}
            onClick={get_christmas_words}
          >
            Submit
          </Button>
        </div>
        <div className="w-50 mg-sm">
          <label>Result:</label>
          <Input.TextArea size="large" value={response} />
        </div>
      </div>
    </div>
  );
}
