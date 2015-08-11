package au.id.cmears.courant;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Intent;
import android.os.Bundle;
import android.os.Environment;
import android.os.StrictMode;
import android.util.Log;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

public class MainActivity extends Activity
{
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        Log.d("MainActivity", "onCreate");
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

        List<Run> runs = RunStorage.getRuns(this);
        LinearLayout runList = (LinearLayout) findViewById(R.id.run_list);

        // Get the directory for the user's public pictures directory.
        File file = new File(Environment.getExternalStoragePublicDirectory(
                Environment.DIRECTORY_PICTURES), "courant-running");
        if (!file.mkdirs()) {
            Log.e("MainAct", "Directory not created");
        }

        PrintWriter fos = null;
        try {
            fos = new PrintWriter(file.toString() + "/" + "output");
        } catch (FileNotFoundException e) {
            Log.e("MainAct", "file not found");
        }

        for (Run r : runs) {
            TextView t = new TextView(this);
            t.setText(r.uuidString());
            runList.addView(t);
            fos.println(r.toJSON());
        }
        //Log.d("MainActivity", "runs: " + runs.toString());
        fos.close();
    }

    public void onStart() {
        Log.d("MainActivity", "onStart");
        super.onStart();
    }

    public void onResume() {
        Log.d("MainActivity", "onResume");
        super.onResume();
    }

    public void onPause() {
        Log.d("MainActivity", "onPause");
        super.onPause();
    }

    public void onStop() {
        Log.d("MainActivity", "onStop");
        super.onStop();
    }

    public void onDestroy() {
        Log.d("MainActivity", "onDestroy");
        super.onDestroy();
    }

    // The user has clicked the "start new run" button.
    public void startNewRun(View view) {
        startRunService();
        Intent intent = new Intent(this, RunInProgressActivity.class);
        startActivity(intent);
    }

    public void startRunService() {
        Log.d("MainActivity", "startRunService");
        Intent intent = new Intent(this, RunService.class);
        ComponentName name = startService(intent);
        if (name == null) {
            Log.d("MainActivity", "Tried to start service but it failed");
        }
    }

    public void uploadAllRuns(View view) {
        try {
            StrictMode.ThreadPolicy policy = new StrictMode.ThreadPolicy.Builder().permitAll().build();
            StrictMode.setThreadPolicy(policy);
            Log.d("Upload", "starting upload");
            List<Run> runs = RunStorage.getRuns(this);
            Log.d("Upload", "got runs");
            URL url = new URL("http://courant.cmears.id.au/postrun");
            Log.d("Upload", "got url");
            for (Run r : runs) {
                Log.d("Upload", "starting upload of run");
                HttpURLConnection c = (HttpURLConnection) url.openConnection();
                Log.d("Upload", "opened connection");
                c.setRequestMethod("POST");
                c.setDoOutput(true);
                try {
                    String postParams = "";
                    postParams += "json=";
                    postParams += r.toJSON().toString();
//                    OutputStream os = new BufferedOutputStream(c.getOutputStream());
//                    os.write("")
//                    os.write(r.toJSON().toString().getBytes());
//                    os.close();
                    c.setFixedLengthStreamingMode(postParams.getBytes().length);
                    PrintWriter out = new PrintWriter(c.getOutputStream());
                    out.print(postParams);
                    out.close();
                    Log.d("Upload", "wrote bytes");
                    Log.d("Upload", "response: " + c.getResponseCode());
                } catch (MalformedURLException e) {
                    Log.d("Upload", "impossible: malformed url: " + e);
                } finally {
                    Log.d("Upload", "disconnecting");
                    c.disconnect();
                }
            }
        } catch (IOException e) {
            Log.d("Upload", "IO exception: " + e);
        }
    }
}
