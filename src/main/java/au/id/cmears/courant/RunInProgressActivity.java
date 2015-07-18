package au.id.cmears.courant;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.speech.tts.TextToSpeech;
import android.speech.tts.Voice;
import android.util.Log;
import android.util.Pair;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.TextView;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Set;


public class RunInProgressActivity extends Activity implements ServiceConnection {

    RunService runService;
    Run run;
    Handler handler;
    boolean tryToUpdate;

    ChartView speedChart;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        speedChart = new ChartView(this);
        speedChart.setMinimumHeight(200);
        speedChart.setMinimumWidth(500);
        setContentView(R.layout.activity_run_in_progress);
        ((LinearLayout) findViewById(R.id.ripaLayout)).addView(speedChart);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_run_in_progress, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    public void onStart() {
        // Whenever the activity is awake, bind to the RunService.
        Intent intent = new Intent(this, RunService.class);
        bindService(intent, this, 0);
        tryToUpdate = true;
        handler = new Handler();
        handler.postDelayed(new UpdateRun(), 100);
        super.onStart();
    }

    private class UpdateRun implements Runnable {
        public void run() {
            if (tryToUpdate) {
//                Log.d("RIPA", "UpdateRun:run");
                updateRun();
                handler.postDelayed(this, 100);
            }
        }
    }

    public void onStop() {
        unbindService(this);
        super.onStop();
    }

    public void stopRun(View view) {
        updateRun();
        tryToUpdate = false;
        recordRun();
        stopService(new Intent(this, RunService.class));
        finish();
    }

    public void toggleChatty(View view) {
        return;
//        if (willingToTalk) {
//            willingToTalk = false;
//            ((Button) findViewById(R.id.toggleChatty)).setText("be chatty");
//        } else {
//            willingToTalk = true;
//            ((Button) findViewById(R.id.toggleChatty)).setText("be quiet");
//        }
    }

    public void updateRun() {
        if (runService != null) {
            run = runService.getCurrentRun();
//            Log.d("RIPA", "current run: " + run.toString());
//            Log.d("RIPA", "json: " + run.toJSON());
            TextView info = (TextView) findViewById(R.id.currentRunInfo);
            int numSamples = run.numSamples();
            info.setText(Integer.toString(numSamples));

            Pair<Double,Double> latestPosition = run.latestPosition();
            Float latestSpeed = run.latestSpeed();
            Float latestBearing = run.latestBearing();
            Float latestAccuracy = run.latestAccuracy();
            Float latestAverageSpeed = run.latestAverageSpeed();
            double totalDistance = run.totalDistance();
            double totalDisplacement = run.totalDisplacement();

            if (latestPosition != null) ((TextView) findViewById(R.id.latestPosition)).
                    setText(Double.toString(latestPosition.first) + " / " + Double.toString(latestPosition.second));
            if (latestSpeed != null) ((TextView) findViewById(R.id.latestSpeed)).setText(Double.toString(latestSpeed));
            if (latestBearing != null) ((TextView) findViewById(R.id.latestBearing)).setText(Double.toString(latestBearing));
            if (latestAccuracy != null) ((TextView) findViewById(R.id.latestAccuracy)).setText(Double.toString(latestAccuracy));
            if (latestAverageSpeed != null) ((TextView) findViewById(R.id.latestAverageSpeed)).setText(Double.toString(latestAverageSpeed));
            ((TextView) findViewById(R.id.totalDisplacement)).setText(Double.toString(totalDisplacement));
            ((TextView) findViewById(R.id.totalDistance)).setText(Double.toString(totalDistance));

            long startTime = run.startTime();
            long now = System.currentTimeMillis();
            long duration = now - startTime;
            ((TextView) findViewById(R.id.runningTime)).setText(Long.toString(duration));

            speedChart.setRun(run);
            speedChart.invalidate();

        } else {
            Log.d("RIPA", "not bound");
        }
    }

    public void recordRun() {
        if (run != null) {
            RunStorage.recordRun(this, run);
        }
    }

    public void onServiceConnected(ComponentName name, IBinder service) {
        RunService.LocalBinder binder = (RunService.LocalBinder) service;
        runService = binder.getService();
    }

    public void onServiceDisconnected(ComponentName name) {
        runService = null;
    }
}

class ChartView extends View {
    Run run;
    ChartView(Context context) {
        super(context);
    }
    void setRun(Run r) { run = r; }
    public void onDraw(Canvas canvas) {
        if (run != null) {
            Paint red = new Paint();
            red.setARGB(255, 255, 0, 0);
            red.setStrokeWidth(5);
            Segment seg = run.getLastSegment();
            if (seg != null) {
                int n = seg.numSamples();
                int start = Math.max(0, n - 100);
                int i = 0;
                while (start+i < n - 1) {
                    canvas.drawLine(5*i, 200-2*seg.samples.get(start+i).getSpeed()*3.6f, 5*(i + 1), 200-2*seg.samples.get(start+i + 1).getSpeed()*3.6f, red);
                    i++;
                }
            }
        }
        super.onDraw(canvas);
    }
}