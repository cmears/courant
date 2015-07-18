package au.id.cmears.courant;

import android.app.Notification;
import android.app.PendingIntent;
import android.app.Service;
import android.app.TaskStackBuilder;
import android.content.Context;
import android.content.Intent;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Binder;
import android.os.Bundle;
import android.os.IBinder;
import android.speech.tts.TextToSpeech;
import android.speech.tts.Voice;
import android.util.Log;
import android.widget.TextView;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Set;

/**
 * Created by chris on 7/07/15.
 */
public class RunService extends Service implements TextToSpeech.OnInitListener {
    public int NOTIFICATION_ID = 1;

    Run currentRun;
    LocationManager locMan;
    LocationListener locListener;

    boolean readyToTalk;
    boolean willingToTalk;
    TextToSpeech tts;

    long previousSpeedUpdate;
    int previousMinuteUpdate;

    public void onInit(int status) {
        if (status == TextToSpeech.SUCCESS) {
            readyToTalk = true;
            Set<Voice> voices = tts.getVoices();
            Voice bestVoice = null;
            for (Voice v : voices) {
                Log.d("RIPA", v.getName());
                if (v.getName().equals("en_GB-locale")) {
                    bestVoice = v;
                    break;
                }
            }
            if (bestVoice != null)
                tts.setVoice(bestVoice);
        }

        willingToTalk = true;
        previousMinuteUpdate = 0;
        previousSpeedUpdate = 0;
    }

    public void updateSpeech() {
        long startTime = currentRun.startTime();
        long now = System.currentTimeMillis();
        long duration = now - startTime;
        Float latestSpeed = currentRun.latestSpeed();

        if (readyToTalk && willingToTalk) {
            int thisMinute = (int)(duration / 60000);
            long timeSinceLastSpeedUpdate = now - previousSpeedUpdate;
            Log.d("RunService", startTime + " " + now + " " + timeSinceLastSpeedUpdate + " " + thisMinute + " " + previousMinuteUpdate + " " + previousSpeedUpdate);
            if (thisMinute != previousMinuteUpdate) {
                tts.speak(Long.toString(thisMinute) + " minutes passed", TextToSpeech.QUEUE_FLUSH, null, "time");
                previousMinuteUpdate = thisMinute;
                previousSpeedUpdate = now;
            } else if (timeSinceLastSpeedUpdate > 15000) {
                if (latestSpeed != null) {
                    BigDecimal bd = (new BigDecimal(latestSpeed)).setScale(1, RoundingMode.HALF_UP);
                    tts.speak(bd.toPlainString(), TextToSpeech.QUEUE_FLUSH, null, "speed");
                    previousSpeedUpdate = now;
                }
            }
        }
    }

    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d("RunService", "onStartCommand");
        // Put this service in the foreground.
        Intent clickIntent = new Intent(this, RunInProgressActivity.class);
        TaskStackBuilder stackBuilder = TaskStackBuilder.create(this);
        stackBuilder.addParentStack(RunInProgressActivity.class);
        stackBuilder.addNextIntent(clickIntent);
        PendingIntent pendingIntent =
                stackBuilder.getPendingIntent(0, PendingIntent.FLAG_UPDATE_CURRENT);
        Notification notification = new Notification.Builder(this)
                .setContentTitle("Run in progress")
                .setContentText("Content text")
                .setContentIntent(pendingIntent)
                .setSmallIcon(R.drawable.ic_launcher)
                .build();
        startForeground(NOTIFICATION_ID, notification);

        // Initialise the run.
        currentRun = new Run();

        // Initialise speech.
        readyToTalk = false;
        willingToTalk = false;
        tts = new TextToSpeech(this, this);

        // Request location updates.
        locMan = (LocationManager) this.getSystemService(Context.LOCATION_SERVICE);
        locListener = new LocationListener() {
            public void onLocationChanged(Location loc) {
                currentRun.newLocation(loc);
                updateSpeech();
            }
            public void onStatusChanged(String provider, int status, Bundle extras) {}
            public void onProviderEnabled(String provider) {}
            public void onProviderDisabled(String provider) {}
        };
        locMan.requestLocationUpdates(LocationManager.GPS_PROVIDER, 100, 0, locListener);

        return START_STICKY;
    }

    private final IBinder binder = new LocalBinder();

    public class LocalBinder extends Binder {
        RunService getService() {
            return RunService.this;
        }
    }

    public IBinder onBind(Intent intent) {
        return binder;
    }

    public void stopCurrentRun() {
        locMan.removeUpdates(locListener);
        tts.shutdown();
    }

    public Run getCurrentRun() {
        return currentRun;
    }

    public void onDestroy() {
        stopCurrentRun();
    }
}
