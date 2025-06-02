import sleap
import cv2
import glob
import os
import numpy as np

import arduino as ard


def check_sleap():
    sleap.disable_preallocation()  # This initializes the GPU and prevents TensorFlow from filling the entire GPU memory
    sleap.versions()
    sleap.system_summary()


def trigger(myArduino, detection_bool):
    if detection_bool:
        myArduino.writeDO_all(1)
    else:
        myArduino.writeDO_all(0)

# SLEAP Camera setting class
class SimulatedCamera:
    """Simulated camera class that serves frames from memory continuously.

    Attributes:
        frames: Numpy array with pre-loaded frames.
        frame_counter: Count of frames that have been grabbed.
    """

    frames: np.ndarray
    frame_counter: int

    def __init__(self, frames):
        self.frames = frames
        self.frame_counter = 0
    
    def grab_frame(self):
        idx = self.frame_counter % len(self.frames)
        self.frame_counter += 1
        return self.frames[idx]
    
# Trigger calculation
def calculate_trigger(result_numpy):
    #基準点はreference pointのyの値の100px下
    if result_numpy.size < 4:
        detection_bool = False
        return detection_bool
    
    if np.any(np.isnan(result_numpy[1, 1])) or np.any(np.isnan(result_numpy[0, 1])):
        detection_bool = False
        return detection_bool
    
    if result_numpy[1, 1].size > 1:
        detection_bool = False
        return detection_bool
    
    
    reference_y = result_numpy[0, 1] + 50
    position_y = result_numpy[1, 1]
    
    if position_y >= reference_y:
        detection_bool = True
    else:
        detection_bool = False
    
    return detection_bool


def real_time_process(predictor, myArduino):
    cap = cv2.VideoCapture(0)
    cap.set(cv2.CAP_PROP_SETTINGS, 1)
    cap.set(cv2.CAP_PROP_FPS, 30)

    
    while True:
        # 1フレームずつ取得する。
        ret, frame = cap.read()
        # prediction
        frame_predictions = predictor.inference_model.predict_on_batch(np.expand_dims(frame, axis=0))
        predict_tensor = frame_predictions["instance_peaks"].to_tensor()
        predict_nuarray = predict_tensor.numpy()
        predict_nuarray = np.squeeze(predict_nuarray)
        # print(predict_nuarray)
        
        # Calculate Trigger
        detection_bool = calculate_trigger(predict_nuarray)
        trigger(myArduino, detection_bool)
        
        
        #フレームが取得できなかった場合は、画面を閉じる
        if not ret:
            print("Not find camera")
            break
    
        # ウィンドウに出力
        cv2.imshow("Frame", frame)
        key = cv2.waitKey(1)
        # Escキーを入力されたら画面を閉じる
        if key == 27:
            break
        
    cap.release()
    cv2.destroyAllWindows()

def main():
    print("Start Project!!")
    check_sleap()
    
    # Load SLEAP Model
    predictor = sleap.load_model(["./segment_LED_SLEAP_analysis/models/250410_182947.centroid.n=200", 
                                  "./segment_LED_SLEAP_analysis/models/250410_183617.centered_instance.n=200"],
                                 batch_size=16)
    print("Complete model load")
    # Load Arduino Firmata
    myArduino = ard.dio(comport="COM3", doCh_IDs=[13])
    print("Complete Arduino load")
    print("Start SLEAP real-time process")
    real_time_process(predictor, myArduino)
    print("Finish Project!!")
    

if __name__ == '__main__':
    main()
    