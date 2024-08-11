import React, { useState, useRef } from "react";

export function JsonUpload({ setJsonData }) {
  const [error, setError] = useState(null);
  const fileInputRef = useRef(null);
  const fileDataRef = useRef(null);

  const handleFileUpload = (event) => {
    const file = event.target.files[0];
    if (file && file.type === "application/json") {
      const reader = new FileReader();
      reader.onload = (e) => {
        try {
          const parsedData = JSON.parse(e.target.result);
          setJsonData(parsedData);
          fileDataRef.current = e.target.result; // Store the file content
        } catch (err) {
          setError("Invalid JSON file");
        }
      };
      reader.readAsText(file);
    } else {
      setError("Please upload a valid JSON file");
    }
  };

  const handleReload = () => {
    if (fileDataRef.current) {
      try {
        const parsedData = JSON.parse(fileDataRef.current);
        setJsonData(parsedData);
        setError(null);
      } catch (err) {
        setError("Failed to reload JSON data");
      }
    }
  };

  return (
    <div>
      <div className="font-bold">{"Upload Family Tree Data"}</div>
      <div className="bg-gray-100 rounded w-fit p-2 border mt-1">
        <input
          type="file"
          accept=".json"
          onChange={handleFileUpload}
          ref={fileInputRef}
        />
        <button
          className="bg-gray-200 px-2 border rounded border-gray-400"
          onClick={handleReload}
          disabled={!fileDataRef.current}
        >
          Reload Data
        </button>

        {error && <p style={{ color: "red" }}>{error}</p>}
      </div>
    </div>
  );
}
